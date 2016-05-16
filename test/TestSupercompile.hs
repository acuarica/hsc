
module Main where

import Util
import Expr
import Parser
import Eval
import Splitter
import Supercompile

-- doSuper :: (String, Expr, Expr) -> (String, Expr, Expr)
-- doSuper (s, expr, expstate) = (s, supercompile expr, supercompile expr)
--
-- doParse :: (String, Expr) -> (String, Expr, Expr)
-- doParse (e, expstate) = (show (parseExpr e), parseExpr e, expstate)

main :: IO ()
main = --doTests (doSuper . doParse) [
    mapM_ (print . supercompile . parseExpr) [
    --mapM_ (print . (\(c,(n,h,p))->p) . runMemo . parseExpr) [
      -- "x",
      -- "Tree",
      -- "Succ 3",
      -- "Succ n",
      -- "f x",
      --"case n of Zero->A B; Succ nn->C D E;"

    "   let inc={n->Succ n}\
    \in let map={f->{xs->case xs of \
    \  Nil->Nil;\
    \  Cons y ys-> Cons (f y) (map f ys);}}\
    \in let cp={f->{g->{x->f (g x)}}} \
    \in map (cp f g) zs"

    -- "let map={f->{xs->case xs of \
    -- \  Nil->Nil; \
    -- \  Cons y ys-> Cons (f y) (map f ys);}} \
    -- \in map g (map f ys)"


    --
    -- ( root =
    -- "let $mapinc={\\$xs-> case $xs of {\
    -- \  Nil->Nil;\
    -- \  Cons $y $ys -> Cons (Succ $y) ($mapinc $ys) ; }}\
    -- \in $mapinc", "A"),
--    ("x", usevar "x")
  ]
