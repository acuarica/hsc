
module Main where

import Util (doTests)
import Expr (Expr(Let))
import Parser (parseExpr)
import Eval (eval)
import Supercompile (supercompile)

main :: IO ()
main = doTests (\(e, f, ar) -> (eval (f (sp e)), (eval . parse) ar)) [
  --("")
  (mapinc, Let "zs" (parse "[]"), "[]"),
  (mapinc, Let "zs" (parse "[1,2,3,4,5]"), "[2,3,4,5,6]"),
  (mapmapincinc, Let "zs" (parse "[]"), "[]")--,
  --(mapmapincinc, Let "zs" (parse "[1]"), "[3]")
  ]
  where
    parse = parseExpr
    sp = supercompile . parse
    mapinc =
      "let inc={n->Succ n}\
      \in let map={f->{xs->case xs of \
      \  Nil->Nil;\
      \  Cons y ys-> Cons (f y) (map f ys);}}\
      \in map inc zs"
    mapmapincinc =
      "let inc={n->Succ n}\
      \in let map={f->{xs->case xs of \
      \  Nil->Nil;\
      \  Cons y ys-> Cons (f y) (map f ys);}}\
      \in map inc (map inc zs)"
