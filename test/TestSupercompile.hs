
module Main where

import Util
import Expr
import Parser
import Eval
import Splitter
import Supercompile

e' = "let map={f->{xs->case xs of \
  \Nil->Nil;Cons y ys-> Cons (f y) (map f ys);}} in map {n->Succ n} zs"

e = "   let inc={n->Succ n}\
 \in let map={f->{xs->case xs of \
 \  Nil->Nil;\
 \  Cons y ys-> Cons (f y) (map f ys);}}\
 \in map inc zs"

parse = parseExpr
sp = supercompile . parse

runmatch x y =
  match (newConf emptyEnv (parseExpr x)) (newConf emptyEnv (parseExpr y))

main :: IO ()
main = do
  -- mapM_ (print  . (\(x,y,v)-> runmatch x y == v) ) [
  --     ("[1,2,3,x]", "[1,2,3,y]", True),
  --     ("let i={n->Succ n} in i a", "let i={n->Succ n} in i b", True)
  --   ]
  doTests (\(e, f, ar) -> (eval (f (sp e)), (eval.parse) ar)) [
     (e, Let "zs" (parse "[1]"), "[2]")
    ]
  mapM_ (print  . gp . runMemo . parseExpr) [
      e
      --"[A,B,C,D]"
    ]
  --mapM_ (print . supercompile . parseExpr) [
    --mapM_ (print . (\(c,(n,h,p))->p) . runMemo . parseExpr) [
      -- "x",
      -- "Tree",
      -- "Succ 3",
      -- "Succ n",
      -- "f x",
      --"case n of Zero->A B; Succ nn->C D E;"

      --e

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
--    ]
