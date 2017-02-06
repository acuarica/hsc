
module Tree
  (Tree(Node), draw)
where

data Tree a e = Node a [(e, Tree a e)]

type Forest a e = [(e, Tree a e)]

draw :: (Show a, Show e) => Tree a e -> String
draw t0 = unlines $ draw' t0
  where
    draw' :: (Show a, Show e) => Tree a e -> [String]
    draw' (Node x ts0) = lines (show x) ++ drawForest ts0

    drawForest :: (Show a, Show e) => Forest a e -> [String]
    drawForest [] = []
    drawForest [(e, t)] = "|" : shift e "`- " "   " (draw' t)
    drawForest ((e, t):ts) = "|" : shift e "+- " "|  " (draw' t) ++ drawForest ts

    shift :: Show e => e -> String -> String -> [String] -> [String]
    shift e first other = zipWith (++) ((first++show e ++ " -> "):repeat other)
