{-# LANGUAGE NoMonomorphismRestriction #-}
{- reference: http://ogdl.sourceforge.net/spec/ogdl-path.htm -}

module Text.OGDL.Path where

import Text.OGDL.Combinators
import Text.OGDL.Parsec (parseOGDL)
import Text.Parsec
import Data.List.Split

-- This is really awful
compilePath = concatMap children . concatMap  (either (const []) return . parseOGDL "") . splitOn "." 

