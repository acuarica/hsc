
module Main where

import Util
import Expr
import Parser
import Eval
import Supercompile

-- doSuper :: (String, Expr, Expr) -> (String, Expr, Expr)
-- doSuper (s, expr, expstate) = (s, supercompile expr, supercompile expr)
--
-- doParse :: (String, Expr) -> (String, Expr, Expr)
-- doParse (e, expstate) = (show (parseExpr e), parseExpr e, expstate)
e = parseExpr "let inc={n->Succ n}\
    \ in let map={f->{xs->case xs of \
    \  Nil->Nil; Cons y ys-> Cons (f y) (map f ys);}}\
    \in map inc ys"

s0 = newState emptyEnv e

main :: IO ()
main = -- putStrLn $ doe e
  --doTests (doSuper . doParse) [
    mapM_ (putStrLn . unwords . map show . split . reduce . newState [] . parseExpr) [
      -- "x",
      -- "Tree",
      -- "Succ 3",
      -- "Succ n",
      -- "f x",
      --"case n of Zero->A B; Succ nn->C D E;"

    "let inc={n->Succ n}\
    \ in let map={f->{xs->case xs of \
    \  Nil->Nil; Cons y ys-> Cons (f y) (map f ys);}}\
    \in map inc zs"


    --
    -- ( root =
    -- "let $mapinc={\\$xs-> case $xs of {\
    -- \  Nil->Nil;\
    -- \  Cons $y $ys -> Cons (Succ $y) ($mapinc $ys) ; }}\
    -- \in $mapinc", "A"),
--    ("x", usevar "x")
  ]
