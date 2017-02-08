
module Tree
  (Tree(Node), depth, draw, flatten)
where

import Control.Arrow (second)

data Tree e a = Node a (Forest e a)

type Forest e a = [(e, Tree e a)]

depth :: Int -> Tree e a -> Tree e a
depth n (Node x ts) = Node x $ if n == 0
  then []
  else map (second $ depth (n - 1)) ts

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

flatten :: Tree e a -> [a]
flatten (Node x ts) = x : concatMap (flatten . snd) ts

instance Functor (Tree e) where
  fmap f (Node x ts) = Node (f x) (map (second $ fmap f) ts)
