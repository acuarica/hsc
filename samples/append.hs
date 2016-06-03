
module Sample where

append :: [a] -> [a] -> [a]
append xs ys = case xs of
  [] -> ys
  (x':xs') -> x': append xs' ys

append3 :: [a] -> [a] -> [a] -> [a]
append3 xs ys zs = append (append xs ys) zs

root = append3
