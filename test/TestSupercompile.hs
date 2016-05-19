
module Main where

import Test.HUnit (Counts, runTestTT, test, (~:), (~?=))
import Control.Arrow (second)

import Expr (Expr(..), appVars)
import Parser (parseExpr)
import Eval (newConf, emptyEnv, eval)
import Supercompile (envToLet, match, supercompile)

main :: IO Counts
main = runTestTT $ test $
  map testMatch1 [
    ("x", "y", True),
    ("f x", "f y", True),
    ("f a b", "f c d", True),
    ("f a a", "f b b", True),
    ("[1,2,3,x]", "[1,2,3,y]", True),
    ("{n->Succ n}", "Succ n", True),
    ("{n->Succ n}", "Succ m", True),
    ("Succ n", "{n->Succ n}", True),
    ("let inc={n->Succ n} in inc x", "let inc={n->Succ n} in inc y", True),
    ("let i={n->Succ n} in i a", "let i={n->Succ n} in i b", True),
    ("let i={n->Succ n} in i", "let i={n->Succ n} in i", True),
    ("let i={n->Succ n} in i", "{n->Succ n}", True),
    ("let i={n->Succ n} in i", "Succ n", True),
    ("let i={n->Succ n} in i",
      "let i={n->Succ n} in let id={x->x} in i m", True)
  ] ++
  map testMatch2 [
    ( (env, [], appVars (Var "map") ["inc", "zs"]),
      (env, [], appVars (Var "map") ["inc", "ys"]), True),
    ( ([], [], envToLet env (appVars (Var "map") ["inc", "zs"])),
      (env, [], appVars (Var "map") ["inc", "ys"]), True),
    ( (env, [], appVars (Var "map") ["inc", "ys"]),
      ([], [], envToLet env (appVars (Var "map") ["inc", "zs"])), True)
  ] ++
  map testSupercompile [
    (mapinczs, Let "zs" (parse "[]"), "[]"),
    (mapinczs, Let "zs" (parse "[1,2,3,4,5]"), "[2,3,4,5,6]"),
    (mapinc, \e-> App e (parse "[]"), "[]"),
    (mapinc, \e-> App e (parse "[1,2,3,4,5]"), "[2,3,4,5,6]")--,
    --(mapincmapinczs, Let "zs" (parse "[]"), "[]"),
    --(mapincmapinczs, Let "zs" (parse "[1]"), "[3]")
  ]
  where
    inc = ("inc", "{n->Succ n}")
    mp = ("map", "{f->{xs->case xs of Nil->[];Cons y ys->Cons (f y) (map f ys);}}")
    env = map (second parseExpr) [inc, mp]

    testMatch1 (x,y,v) = "match1" ~: x ++ " =~= " ++ y ~: match (s x) (s y) ~?= v
    testMatch2 (l,r,v) = "match1" ~: show l ++ " =~= " ++ show r ~: l `match` r ~?= v
    s = newConf emptyEnv . parseExpr
    testSupercompile (e, f, ar) = eval (f (sp e)) ~?= (eval . parse) ar
    parse = parseExpr
    sp = supercompile . parse
    mapinczs =
      "let inc={n->Succ n}\
      \in let map={f->{xs->case xs of \
      \  Nil->Nil;\
      \  Cons y ys-> Cons (f y) (map f ys);}}\
      \in map inc zs"
    mapinc =
      "let inc={n->Succ n}\
      \in let map={f->{xs->case xs of \
      \  Nil->Nil;\
      \  Cons y ys-> Cons (f y) (map f ys);}}\
      \in map inc"
    mapincmapinczs =
      "let inc={n->Succ n}\
      \in let map={f->{xs->case xs of \
      \  Nil->Nil;\
      \  Cons y ys-> Cons (f y) (map f ys);}}\
      \in map inc (map inc zs)"
