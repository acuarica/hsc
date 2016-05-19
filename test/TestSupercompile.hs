
module Main where

import Test.HUnit

import Expr (Expr(Let))
import Parser (parseExpr)
import Eval (newConf, emptyEnv, eval)
import Supercompile (match, supercompile)

main :: IO Counts
main = runTestTT $ test $
  map testMatch [
    ("x", "y", True),
    ("[1,2,3,x]", "[1,2,3,y]", True),
    ("{n->Succ n}", "Succ n", True),
    ("{n->Succ n}", "Succ m", True),
    ("let inc={n->Succ n} in inc x", "let inc={n->Succ n} in inc y", True),
    ("let i={n->Succ n} in i a", "let i={n->Succ n} in i b", True),
    ("let i={n->Succ n} in i", "let i={n->Succ n} in i", True),
    ("let i={n->Succ n} in i", "{n->Succ n}", True),
    ("let i={n->Succ n} in i", "Succ n", True)
    -- ("let i={n->Succ n} in i",
    --   "let i={n->Succ n} in let id={x->x} in i m", True)
  ] ++
  map testSupercompile [
    (mapinc, Let "zs" (parse "[]"), "[]")
    --(mapinc, Let "zs" (parse "[1,2,3,4,5]"), "[2,3,4,5,6]")--,
  --(mapmapincinc, Let "zs" (parse "[]"), "[]")--,
  --(mapmapincinc, Let "zs" (parse "[1]"), "[3]")
  ]
  where
    testMatch (x,y,v) = "match" ~: x ++ " =~= " ++ y ~: match (s x) (s y) ~?= v
    s = newConf emptyEnv . parseExpr
    testSupercompile (e, f, ar) = eval (f (sp e)) ~=? (eval . parse) ar
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
