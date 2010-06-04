{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{- reference: http://ogdl.sourceforge.net/spec/ogdl-path.htm -}

module Text.OGDL.Path where

import Safe ( atMay )
import Data.Tree ( Tree(..) )
import Data.Maybe ( catMaybes, maybeToList )
import Data.List (intercalate,genericDrop)
import Text.Regex.TDFA ((=~))
import Text.Parsec
    ( char,
      digit,
      noneOf,
      string,
      between,
      choice,
      many1,
      option,
      sepBy1,
      (<?>),
      parse,
      try )

class Query c where
  select :: c -> [Tree String] -> [Tree String] 
  selectList :: [c] -> [Tree String] -> [Tree String] 
  selectList cs ts = concatMap (ts//) cs

data Glob = AnyNode | AnyPath 

data Query a => Descendant a = WithChild a

data (Query a,Query b) => Path a b = a :> b

instance Query Integer where 
  select c = return . head . genericDrop c

--instance Query String where
--  select "" ts = []
--  select c ts = concatMap subForest ( filter ( (== c) . rootLabel)  ts )
--  select c = select (==c)

instance Query Char where
  select c ts = selectList [c] ts
  selectList cs ts = select (==cs) ts 

instance Query (String -> Bool) where
  select c = concatMap subForest . filter ( c . rootLabel )

instance Query (Tree String -> Bool) where
  select c = filter c 

instance Query a => Query (Maybe a) where
  select Nothing = const []
  select (Just c) = select c

instance Query a => Query [a] where
  select = selectList 

instance Query Glob where
  select AnyNode ts = concatMap subForest ts
  select AnyPath ts = concatMap trees ts
    where trees root@(Node _ ts) = root:(concatMap trees ts)

instance Query a => Query (Descendant a) where
  select (WithChild a) ts = filter ( not . null . select a . subForest) ts

instance (Query a, Query b) => Query (Path a b) where
  select (qa :> qb) ts = ts // qa // qb

-- for this to work, I need to return the root node and not just
-- the children

(//) :: Query c => [Tree String] -> c -> [Tree String]
ts // c = select c ts



match :: String -> String -> Bool
match pat str =  str =~ pat

combine c1 c2 ts = ts // c1 ++ ts // c2

