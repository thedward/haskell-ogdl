{-# LANGUAGE FlexibleInstances,MultiParamTypeClasses,TypeSynonymInstances #-}
{- reference: http://ogdl.sourceforge.net/spec/ogdl-path.htm -}

module Text.OGDL.Combinators where

import Data.Tree ( Tree(..) )

infixl 6 `with`, `without`
infixr 5 //, `union`
infixl 5 />, </
infixl 4 <!,!>

type Selector = Tree String -> [Tree String]

class Query q  where
  makeSelector :: q -> Selector
  listSelector :: [q] -> Selector
  listSelector = cat . map makeSelector


instance Query (Selector) where
  makeSelector = id

instance Query Char where
  makeSelector = makeSelector . \x -> [x]
  listSelector = literal

instance Query (Tree String -> Bool) where
  makeSelector = predicate

instance Query (String -> Bool) where
  makeSelector q = predicate (q . label)

instance Query q => Query (Tree q) where
  makeSelector (Node x []) = makeSelector x
  makeSelector (Node x ts) = (makeSelector x) `with` (keep /> (cat $ map makeSelector ts))

instance Query Integer where
  makeSelector n = return . head . drop (fromIntegral n) . children

instance Query a => Query ([a]) where
  makeSelector = listSelector

(//) :: (Query f,Query g)  => f -> g -> Selector
f // g =  concatMap (makeSelector g) . (makeSelector f)

with :: (Query f,Query g) => f -> g -> Selector
f `with` g = filter ( not . null . makeSelector g )  .  (makeSelector f)

without :: (Query f,Query g) => f -> g -> Selector
f `without` g = filter ( null . makeSelector g )  .  (makeSelector f)

(</) :: (Query f,Query g) => f -> g -> Selector
f </ g = (makeSelector f) `with` (children // g)

(/>) :: (Query f,Query g) => f -> g -> Selector
f /> g = (makeSelector f) // children // g

(|>|) :: (a->[b]) -> (a->[b]) -> (a->[b])
f |>| g = \x-> let fx = f x in if null fx then g x else fx

(!>) :: (Query f) => f -> Int -> Selector
f !> n = f // position n children

(<!) :: (Query f) => f -> Int -> Selector
f <! n = position n (makeSelector f)

union :: (a->[b]) -> (a->[b]) -> (a->[b])
union = lift (++)		-- in Haskell 98:   union = lift List.union
  where
    lift :: (a->b->d) -> (c->a) -> (c->b) -> c -> d
    lift f g h = \x-> f (g x) (h x)

cat :: [a->[b]] -> (a->[b])
cat [] = const []
cat fs = foldr1 union fs

deep, deepest, multi :: Query f => f -> Selector
deep f     = q |>| (children // deep q)
  where q = makeSelector f
deepest f  = (children // deepest q) |>| q
  where q = makeSelector f
multi f    = q `union` (children // multi q)
  where q = makeSelector f

keep :: Selector
keep t = [t]

none :: Selector
none = const []

children :: Selector
children = subForest

position :: Query f => Int -> f -> Selector
position n f = nth n . (makeSelector f)
  where nth n xs = case drop n xs of
                        [] -> []
                        (x:_) -> [x]

label :: Tree String -> String
label = rootLabel

literal :: String -> Selector
literal = makeSelector . (==)

predicate p x = filter p [x]

