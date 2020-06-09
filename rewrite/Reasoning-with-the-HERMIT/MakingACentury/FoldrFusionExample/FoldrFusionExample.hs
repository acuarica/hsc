-- | foldr fusion
--
-- strict f    /\    f a = b    /\    forall x y. f (g x y) = h x (f y)
-----------------------------------------------------------------------
--           f . foldr g a = foldr h b
--

module FoldrFusionExample where

{-# RULES "foldr-fusion-example" [~]
  f . foldr g a = foldr h b  #-}

data A = A1 | A2
data B = B1 | B2

f :: A -> B
f A1 = B1
f A2 = B2

g :: Int -> A -> A
g n a0 = if n > 0 then A2 else a0

h :: Int -> B -> B
h n b0 = if n > 0 then B2 else b0

a :: A
a = A1

b :: B
b = B1

-- Objective: rewrite this to foldr h b
expr :: [Int] -> B
expr = f . foldr g a
