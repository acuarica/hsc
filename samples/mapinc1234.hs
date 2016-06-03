
module Sample where

data Nat = Zero | Succ Nat

data List a = Nil | Cons a (List a)

n :: Int -> Nat
n 0 = Zero
n s = Succ (n (s-1))

inc :: Nat -> Nat
inc n = Succ n

lmap :: (a -> b) -> List a -> List b
lmap f xs = case xs of
  Nil -> Nil
  Cons y ys -> Cons (f y) (lmap f ys)

mapinc1234 :: List Nat
mapinc1234 = lmap inc $ Cons (n 1) (Cons (n 2) (Cons (n 3) (Cons (n 4) Nil)))

root = mapinc1234
