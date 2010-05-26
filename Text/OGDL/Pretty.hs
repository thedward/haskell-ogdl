module Text.OGDL.Pretty where

import Text.OGDL.Tree hiding (Tree)
import Text.PrettyPrint
import Data.Tree

ppNode (Word t) = text t

ppNode (Block b) = char '\\' $+$ (nest 2 (text b))

ppNode (Quoted q) = quotes . text $ q

ppNode (Reference r) = braces . text $ r

ppTree (Node n ((Node b@(Block _) _ ):_)) = (ppNode n) <+> (ppNode b)

ppTree (Node n ns) = (ppNode n) $$ (nest 4 (vcat (map ppTree ns)))
