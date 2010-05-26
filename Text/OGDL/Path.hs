{-# LANGUAGE NoMonomorphismRestriction #-}
{- reference: http://ogdl.sourceforge.net/spec/ogdl-path.htm -}

module Text.OGDL.Path where

import Safe(atMay)
import Data.Tree(Tree(..))
import Data.Maybe(Maybe,fromMaybe, maybe, maybeToList, catMaybes)
import Data.List(intercalate)
import Text.Parsec

data Choice = Index Int | Select Int | AnyPath | AnyNode | All deriving (Eq,Show)

--instance Show Choice where
-- show (Index  n) = '[' : shows n "]"
-- show (Select 0) = ""
-- show (Select n) = '{' : shows n "}"
-- show All        = "{}"
-- show AnyNode    = "[*]"
-- show AnyPath    = "[**]"

--data Element = Element (Maybe String) Choice | ArgList [Path]

data Element = Element (Maybe String) Choice deriving (Eq,Show)

--instance Show Element where
--  show (Element s c) = (fromMaybe "" s) ++ (show c)
--  showList es = showString $ intercalate "." (map show es)

type Path = [Element]

subForests (Node _ []) = []
subForests (Node _ ts) = ts ++ concatMap subForests ts


gmap :: Element -> [Tree String] -> [Tree String]

withLabel f t = f (rootLabel t)

gmap (Element label c) ts = do cs <- map subForest ts
                               let ms = maybe cs (flip filter cs . withLabel . (==) ) label
                               case c of
                                    (Select n) -> maybeToList $ ms `atMay` n
                                    All        -> ms
                                    (Index n)  -> catMaybes $ map ( flip atMay n . subForest ) ms
                                    AnyNode    -> concatMap subForest ms
                                    AnyPath    -> concatMap subForests ms

gpath [] ts = ts
gpath (e:es) ts = gpath es (gmap e ts)

readPath :: String -> Either String Path
readPath s = case (parse path s s) of
                  Left e  -> Left (show e)
                  Right p -> Right p

extract t = rootLabel t

--[1] path ::= element ( '.' element )*

path = sepBy1 element (char '.')

--[2] element ::= (string (index|selector)?) | arglist | index | selector

element = do w <- option Nothing $ try ( many1 ( noneOf "[](){},.") ) >>= return . Just
             c <- option (Select 0) $ choice [index,selector]
             return (Element w c)

--[3] index ::= '[' ( number | '*' | '**')? ']'

--  where [n] selects the nth subnode,
--        [**] means any path,
--        [*] means any node

index = ( between (char '[') (char ']') (choice [num,anyNode,anyPath]) )  <?> "index"
        where num = ( do n <- try $ many1 digit
                         return ( Index (read n) ) ) <?> "index number"
              anyPath = do try ( string "**" ) ; return AnyPath
              anyNode = do try (string "*") ; return AnyNode

--[4] selector ::= '{' number? '}'

selector = ( between (char '{') (char '}') $ choice [num,allNodes] ) <?> "selector"
           where num = do n <- try $ many1 digit
                          return ( Select (read n) ) <?> "selector number"
                 allNodes = return All


--[5] arglist ::= '(' (path ( ',' path )* )?  ')'
