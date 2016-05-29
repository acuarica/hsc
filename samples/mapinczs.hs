
data Nat = Zero | Succ Nat

data List a = Nil | Cons a (List a)

inc :: Nat -> Nat
inc n = Succ n

map :: (a -> b) -> List a -> List b
map f xs = case xs of
  Nil -> Nil
  Cons y ys -> Cons (f y) (map f ys)

root :: List a -> List a
root zs = map inc zs
