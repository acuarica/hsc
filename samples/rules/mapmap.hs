
module MapMap where

{-# RULES
  "lmap/lmap"    forall f g xs.  lmap f (lmap g xs) = lmap (f.g) xs
  #-}

data Nat = Zero | Succ Nat

data List a = Nil | Cons a (List a)

inc :: Nat -> Nat
inc = Succ

lmap :: (a -> b) -> List a -> List b
lmap f xs = case xs of
  Nil -> Nil
  Cons y ys -> Cons (f y) (lmap f ys)

root :: (b -> c) -> (a -> b) -> List a -> List c
root f g xs = lmap f (lmap g xs)
