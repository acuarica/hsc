
module Sample where

data Nat = Zero | Succ Nat

data List a = Nil | Cons a (List a)

inc :: Nat -> Nat
inc n = Succ n

lmap :: (a -> b) -> List a -> List b
lmap f xs = case xs of
  Nil -> Nil
  Cons y ys -> Cons (f y) (lmap f ys)

mapinc :: List Nat -> List Nat
mapinc zs = lmap inc zs

root = mapinc
