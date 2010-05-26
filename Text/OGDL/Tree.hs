module Text.OGDL.Tree (Node(..),Tree) where

import qualified Data.Tree as T


data Node = Word String | Quoted String | Block String | Reference String

type Tree = T.Tree Node

instance Show Node where
  show (Word s) = s
  show (Quoted s) = q (concatMap escape s)
                    where escape '"' = "\\\""
                          escape '\\' = "\\\\"
                          escape c = [c]
                          q cs = "\"" ++ cs ++ "\""
  show (Block s) = "\\\n" ++ s
  show (Reference s) = "{" ++ s ++ "}"

