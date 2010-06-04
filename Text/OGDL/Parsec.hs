{-# LANGUAGE NoMonomorphismRestriction #-}

module Text.OGDL.Parsec where

import Text.Parsec(Column, sourceColumn, (<?>), (<|>), getPosition, getState,
                   many, modifyState, runParser, try, unexpected, char, oneOf, satisfy, string,
                   between, choice, count, lookAhead, many1, option, optional, sepEndBy, sepEndBy1)
import Data.Tree(Tree(Node))
import Control.Monad(when, unless, liftM, liftM2)
import Data.List(intercalate)
import Data.Maybe(Maybe(..), fromMaybe)

data IndentChar = Space | Tab deriving (Show,Eq)

data Indentation = I { iStack :: [Column], indentChar :: Maybe IndentChar}
                   deriving (Eq,Show)

emptyState = I [] Nothing

modifyStack f = modifyState (`modifyStack'` f)
                where rec `modifyStack'` f = rec { iStack = f (iStack rec) }

withStack f = getState >>= return . (`withStack'` f)
              where rec `withStack'` f = f (iStack rec)

pushStack c = modifyStack (c:)

popStack = do is <- getState >>= return . iStack
              case is of
                   []    -> return Nothing
                   (x:_) -> modifyStack tail >> return (Just x)

topStack = do is <- getState >>= return . iStack
              case is of
                   []    -> return Nothing
                   (x:_) -> return (Just x)

getIndentChar = liftM indentChar getState

setIndentChar c = modifyState (\x -> x { indentChar = Just c }) >> return c

indent = do cc <- getPosition >>= return . sourceColumn
            pc <- liftM (fromMaybe 0) topStack
            if cc > pc
               then pushStack cc
               else unexpected "end of block"


-- // # 5. Level 1: Tree grammar

-- //     [1] char_word ::= [0x21-0xD7FF] | [0xE000-0xFFFD] | [0x10000-0x10FFFD]

-- // This range includes all Unicode characters, except characters below
-- // and including 0x20 (space), Unicode surrogate blocks, 0xfffe and
-- // 0xffff.

-- Since this actually differs from the characters that can appear
-- in words, I've renamed it.

isAllowedChar = flip (any . inRange)   [ ('\x21','\xD7FF') , ('\xE000','\xFFFD') , ('\x10000','\x10FFFD')  ]
             where i `inRange` (m,n) = m <= i && i <= n

allowedChar = satisfy isAllowedChar

headWordChar = lookAhead (satisfy $ (`notElem` "\"'#(,)") ) >> (satisfy isAllowedChar)

tailWordChar = lookAhead (satisfy $ (`notElem` "\"#(,)") ) >> (satisfy isAllowedChar)

-- //     [2] char_space ::= 0x20 | 0x09

isSpaceChar c = c == '\x20' || c == '\x09'

spaceChar = oneOf "\x20\x09"

spaceChars = many1 spaceChar

trimSpaces p = do r <- p
                  optional spaceChars
                  return r

indentTab = char '\x09' >> return Tab <?> "tab"

indentSpace = char '\x20' >> return Space <?> "space"

indentSpaceChar = indentSpace <|> indentTab

-- //     [3] char_break ::= 0x0d | 0x0a

breakChar = oneOf "\x0d\x0a"

isBreakChar c = c == '\x0d' || c == '\x0a'

-- //     [4] char_end ::= any_char - char_word - char_space - char_break

-- // This production states that any character that is not char\_word,
-- // char\_space or char\_break is considered the end of the OGDL
-- // stream, and makes the parser stop and return, without signaling an
-- // error.

endChar = satisfy (\c -> not (any ($c) [isAllowedChar,isSpaceChar,isBreakChar] ) )

-- //     [5] word ::= ( char_word - ',' - '(' - ')' )+

-- for comments to work, either (a) '#' can't appear in words or
-- (b) it can't appear at the beginning of words and must be separated
-- by a space to start a comment. I'll go with option (a).
-- On a related note, why is char_word called that if it doesn't
-- actually specify the characters that can appear in words?

word = liftM2 (:) (try headWordChar) (option [] $ many1 (try tailWordChar))

-- //     [6] break ::= 0x0a | 0x0d | (0x0d 0x0a)

lineBreak = (choice $ map (try . string) ["\x0d\x0a","\x0a","\x0d"]) <?> "line break"

-- // When parsing, breaks are normalized: standalone 0x0d characters and
-- // 0x0d 0x0a sequences are converted to 0x0a characters. The emitter
-- // should print breaks according to the particular operating system
-- // rules.

-- //     [7] end ::= char_end | ( break "--" break )

end = choice [ endChar >> return (), between lineBreak lineBreak $ string "--" >> return ()]

-- //     [8] space ::= char_space+


-- //     [9] space(n) ::= char_space*n, where n is the equivalent number of spaces.

-- indentWhiteSpace
--   = do i <- liftM indentChar getState
--        maybe (do sc <- indentSpaceChar
--                  modifyState (\x -> x { indentChar = Just sc } ) ) only i
--        optional indentWhiteSpace
--        optional (comment >> optional lineBreak >> whiteSpace)
--     where
--     only p = do { s <- indentSpaceChar ;
--                 when (p /= s) (fail "mixing spaces and tabs is not permitted")
--                 return ()
--

indentation = do cc <- liftM sourceColumn getPosition
                 unless (cc==1) (fail "indetation can only occur at the beginning of a line.")
                 m <- getIndentChar
                 ic <- case m of
                            Nothing -> lookAhead indentSpaceChar >>= setIndentChar
                            Just c -> return c
                 only ic
                 return ()
                 where only p = many ( do s <- indentSpaceChar
                                          when (p /= s) (unexpected "mixing of space and tabs")
                                          return () )

whiteSpace = optional $ many1 ( try comment <|> (spaceChars >> return ()) <|> (lineBreak >> return ()) ) >> return ()

-- // This is the indentation production. It corresponds to the
-- // equivalent number of spaces between the start of a line and the
-- // beginning of the first scalar node. For any two consecutive scalars
-- // preceded by indentation, the second is child of the first one if it
-- // is more indented. Intermixing of spaces and tabs is NOT allowed:
-- // either tabs or spaces should be used for indentation within a
-- // document.

-- //     [10] single_quoted ::= "'" (char_word | char_space | break)* "'"

-- // A quote character that has to be included in the string should be
-- // preceded by '\\'. If the string contains line breaks, leading
-- // spaces on each new line are stripped off. The initial indentation
-- // is defined by the first line after a break. The indentation is
-- // decreased if word characters appear at a lower indentation, but it
-- // is never increased. Lines ending with '\\' are concatenaded. Escape
-- // sequences that are recognized are \\", \\' and \\\\. The character
-- // '\\' should be considered literal in other cases.

-- //     [11] double_quoted ::= '"' (char_word | char_space | break)* '"'

-- // Same rule applies as for [10].

-- FIXME: I still need to handle the described indentation rule


quotedBy q = between qp (qp <?> "end of string") quotedText

  where qp = char q
        quotedText = many ( try escape <|> try stringCharacter )
        stringCharacter = satisfy (\c -> (c/=q) && ( isAllowedChar c || isSpaceChar c) )
        escape = do char '\\'
                    oneOf "\"'\\" <|> (lineBreak >> return ' ')

singleQuoted = quotedBy '\''

doubleQuoted = quotedBy '"'

quoted = (doubleQuoted <|> singleQuoted) <?> "string literal"

-- //     [12] comment ::= '#' (char_word | char_space)+

comment = (char '#' >> many ( allowedChar <|> spaceChar ) >> lineBreak >> return ()) <?> "comment"

-- //     [13] scalar ::= (word | single_quoted | double_quoted )

-- no quoted yet

scalar = (trimSpaces $ word <|> quoted) <?> "scalar"

-- //     [14] block(n) ::= scalar '\' space? break (space(>n) (char_word | char_space)* break)+

-- // A block is a scalar leaf node, i.e., it cannot be parent of other
-- // nodes. It is to be used for holding a block of literal text. The
-- // only transformation that it undergoes is leading space stripping,
-- // according to the indentation rules. A block is child of the scalar
-- // that starts it.

block = do s <- scalar
           char '\\' >> many spaceChar >> lineBreak
           d <- liftM length (many spaceChar)
           l <- many (allowedChar <|> spaceChar)
           lineBreak
           ls <-  try (count d spaceChar >> many ( allowedChar <|> spaceChar )) `sepEndBy` lineBreak
           let txt = intercalate "\n" (l:ls) 
           return $ [Node s [(Node txt [] )] ]

-- //     [15] list ::= (scalar|group) ( (space? ',')? space? (scalar|group) )*

-- this doesn't seem accurate, or terribly useful, let me try:

-- [15.1] node ::= scalar ( (space* group) | (space+ node) )
--
-- [15.2] list ::= (node|group) ( (space* ',' ) (node|group) )*

comma = (trimSpaces $ char ',') <?> "comma"

parens p = between o c p
  where o = trimSpaces $ char '('
        c = trimSpaces $ char ')'

node = try block <|> trimSpaces
       ( do nodeLabel <- scalar
            subNodes <- option [] $ try (node <|> group)
            return [(Node nodeLabel subNodes) ]
       )

-- the grammer doesn't seem to allow lists that end in commas, but
-- such lists appear in the examples.

list = (trimSpaces $ (node <|> group) `sepEndBy1` comma >>= return . concat) <?> "list"

-- //     [16] group ::= '(' space? list?  space? ')'

group = (trimSpaces $ parens (try list <|> return [])) <?> "group"


-- //     [17] line(n) ::=
-- //             space(n) (list|group)?  space? comment? break  |
-- //             space? comment? break |
-- //             space(n) block

-- In order to parse this structurally, I am going to parse a
-- line and all its children in one go, so my function will
-- be called 'tree' instead of 'line'

tree = do whiteSpace
          indent <?> "correct indentation"
          ts <- list
          whiteSpace
          sf <- many (try tree) >>= return . concat
          popStack
          case ts of
               [] -> unexpected "lack of nodes"
               ((Node x ys):zs) -> return ((Node x (ys++sf)):zs)

-- //     [18] graph ::= line* end

--ogdl = liftM (ogdlRoot . concat) $ tree `sepEndBy1` whiteSpace
--  where ogdlRoot xs = [Node "OGDL" xs]

ogdl = liftM concat $ tree `sepEndBy1` whiteSpace

parseOGDL filename source = runParser ogdl emptyState filename source

parseOGDLFromFile fname = do input <- readFile fname
                             return (runParser ogdl emptyState fname input)

-- // # 6. Level 2: Graph grammar

-- //     [19] anchor ::= '-' '{' word '}'

-- //     [20] reference ::= '+' '{' word '}'

-- //     [21] path_reference ::= '=' '{' path '}'

-- // Path syntax should be according to the OGDL Path reference.

-- // # 7. Character encoding

-- // OGDL streams must parse well without explicit encoding information
-- // for all ASCII transparent encodings.

-- // All special characters used in OGDL that define structure and
-- // delimit tokens are part of the US-ASCII (ISO646-US) set. It is
-- // specified that, in unidentified 8-bit streams without a
-- // [Unicode BOM](http://www.unicode.org/unicode/faq/utf_bom.html),
-- // there can be no ASCII values that don't map to ASCII characters,
-- // i.e, should be ASCII transparent. This guarantees that tools that
-- // support only single byte streams will work on any 8-bit fixed or
-- // variable length encoded stream, particularly
-- // [UTF-8](ftp://ftp.rfc-editor.org/in-notes/rfc2279.txt) and most
-- // ISO8859 variants.

-- // When a Unicode BOM is present, then the parser should interpret the
-- // stream as Unicode and choose the right UTF transform.

-- // # 8. Meta-information

-- // The '\#?' character combination used as a top level node (not
-- // necessarily the first one) is reserved for comunication between the
-- // OGDL stream and the parser. It is not mandatory and allows for
-- // future enhancements of the standard. For example, some optional
-- // behavior could be switched on. Normally meta-information will not
-- // be part of the in-memory graph. Meta-information is written in
-- // OGDL, as can be seen in the following examples.

-- //     #? ogdl 1.0

-- //     #? ( ogdl 1.0, encoding iso-8859-1 )

-- // The meta-information keys that are currently reserved are: ogdl,
-- // encoding and schema.

-- // # 9. Round-tripping

-- // OGDL streams are guaranted to round-trip in the presence of a
-- // capable parser and emitter, while maintaining a simple in-memory
-- // structure of nested nodes. Such a parser includes meta-information
-- // but not comments. Depending on the precision of the parser-emitter
-- // chain, the resulting stream may differ from the original in format
-- // or not.
