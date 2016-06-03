
module Main (main) where

import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Control.Arrow (second)

import Expr (Expr(..), appVars)
import Parser (parseExpr)
import Eval (Env, newConf, emptyEnv, eval)
import Match

testMatch :: TestTree
testMatch = testGroup "match1 ~~>" $
  map (\(x,y,v) ->
    testCase (x ++ " =~= " ++ y) $
      match (s x) (s y) @?= v)
  [
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
  ]
  where s = newConf emptyEnv . parseExpr

testMatch2 :: TestTree
testMatch2 = testGroup "match2 ~~>" $
  map (\(l,r,v) ->
    testCase (show l ++ " =~= " ++ show "") $
      l `match` r @?= v)
  [
    ( (env, [], appVars (Var "map") ["inc", "zs"]),
      (env, [], appVars (Var "map") ["inc", "ys"]), True),
    ( ([], [], envToLet env (appVars (Var "map") ["inc", "zs"])),
      (env, [], appVars (Var "map") ["inc", "ys"]), True),
    ( (env, [], appVars (Var "map") ["inc", "ys"]),
      ([], [], envToLet env (appVars (Var "map") ["inc", "zs"])), True)
  ]

testTextualMatch :: TestTree
testTextualMatch = testGroup "matchTextual ~~>" $
  map (\(l,r,v) ->
    testCase (show l ++ " =~= " ++ show "") $
      l `textualMatch` r @?= v)
  [
    ( (env, [], appVars (Var "map") ["inc", "zs"]),
      (env, [], appVars (Var "map") ["inc", "ys"]), True),
    ( ([], [], envToLet env (appVars (Var "map") ["inc", "zs"])),
      (env, [], appVars (Var "map") ["inc", "ys"]), True),
    ( (env, [], appVars (Var "map") ["inc", "ys"]),
      ([], [], envToLet env (appVars (Var "map") ["inc", "zs"])), True)
  ]

env :: Env
env = map (second parseExpr)
  [
  ("inc", "{n->Succ n}"),
  ("map", "{f->{xs->case xs of Nil->[];Cons y ys->Cons (f y) (map f ys);}}")
  ]

main :: IO ()
main = defaultMain $
  testGroup "Supercompiler: match, supercompile/eval"
    [
      testMatch,
      testMatch2,
      testTextualMatch
    ]
