{-# LANGUAGE NoMonomorphismRestriction #-}
{- reference: http://ogdl.sourceforge.net/spec/ogdl-path.htm -}

module Text.OGDL.Path where

import Prelude hiding (read)
import Text.OGDL.Combinators
import Text.OGDL.Parsec (ogdl,pathString)
import Text.Parsec
import Data.List.Split
import Data.Char (isDigit)
import Safe.Failure(read)
import Control.Monad (liftM)

-- This is really awful
--compilePath = concatMap children . concatMap  (either (const []) return . parseOGDL "") . splitOn "." 

pathWord = do p <- pathString
              idx <- optionMaybe index 
              return (maybe (makeSelector p) ($p) idx)

element = try arglist <|> try pathWord

arglist = liftM (makeSelector . children) $ between (char '(') (char ')') ogdl

            
path = do ps <-  element `sepBy1` (char '.')
          return $ foldl (/>) keep ps

index = do i <- between (char '[') (char ']') (many ( digit <|> char '*') )
           case i of
                "*"  -> return ( // children) 
                "**" -> return multi
                ds   -> maybe (fail "bad index") (return . childn) (read ds :: Maybe Int) 
           where childn :: (Query f) => Int -> f -> Selector
                 childn n = (!>n)





