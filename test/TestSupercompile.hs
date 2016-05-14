
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
e = parseExpr "let inc={n->Succ n}\
    \ in let map={f->{xs->case xs of \
    \  Nil->Nil; Cons y ys-> Cons (f y) (map f ys);}}\
    \in map inc zs"

c0 = newConf emptyEnv e
s0 = memo (return c0)
sn = run s0 (0, [])
--fromMemo (c, _) = c

--cn = fromMemo sn

main :: IO ()
main = --doTests (doSuper . doParse) [
    mapM_ (print . runMemo . parseExpr) [
      -- "x",
      -- "Tree",
      -- "Succ 3",
      -- "Succ n",
      -- "f x",
      --"case n of Zero->A B; Succ nn->C D E;"

    "let inc={n->Succ n} in let map={f->{xs->case xs of \
    \  Nil->Nil; Cons y ys-> Cons (f y) (map f ys);}} in map inc zs"

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
