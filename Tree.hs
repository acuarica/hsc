
{-|
  Module for working with labeled trees.
  In particular this module implements with Rose Trees.
-}
module Tree
  (Tree(Node), Forest, depth, draw, flatten)
where

import Control.Arrow (second)

{-|
  Tree data structure.
  The type parameter @e@ represents the type of the edges while @a@ represents the type of the nodes.
-}
data Tree e a = Node a (Forest e a)

{-|
  A Forest is a list of @Tree@s together with edges.
-}
type Forest e a = [(e, Tree e a)]

{-|
  Given a non-negative number @n@, returns the same Tree but up to depth @n@.
-}
depth :: Int -> Tree e a -> Tree e a
depth n (Node x ts) = Node x $ if n == 0
  then []
  else map (second $ depth (n - 1)) ts

{-|
  Converts the Tree to a string that is suitable for printing.
-}
draw :: (Show e, Show a) => Tree e a -> String
draw = unlines . draw'
  where
    draw' :: (Show e, Show a) => Tree e a -> [String]
    draw' (Node x ts) = show x : drawForest ts
    drawForest :: (Show e, Show a) => Forest e a -> [String]
    drawForest [] = []
    drawForest [(e, t)] = "|" : shift e "`- " "   " (draw' t)
    drawForest ((e, t):ts) = "|" : shift e "+- " "|  " (draw' t) ++ drawForest ts
    shift :: Show e => e -> String -> String -> [String] -> [String]
    shift e first other = zipWith (++) ((first++show e ++ " -> "):repeat other)

{-|
  Converts the Tree to a list.
  It does not use the edges.
-}
flatten :: Tree e a -> [a]
flatten (Node x ts) = x : concatMap (flatten . snd) ts

instance Functor (Tree e) where
  fmap f (Node x ts) = Node (f x) (map (second $ fmap f) ts)
