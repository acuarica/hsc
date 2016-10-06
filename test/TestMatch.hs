
module Main (main) where

import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Control.Arrow (second)

import Expr (Expr(Var, Con, App), app, appVars, substAlts)
import Parser (parseExpr)
import Eval (Env, newConf, emptyEnv, eval)
import Match (match, envExpr, (|~~|), (<|), (|><|))

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
  ("let i={n->S n} in i", "let i={n->S n} in let id={x->x} in i m", True)
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
    ( ([], [], envExpr (env, [], appVars (Var "map") ["inc", "zs"])),
      (env, [], appVars (Var "map") ["inc", "ys"]), True),
    ( (env, [], appVars (Var "map") ["inc", "ys"]),
      ([], [],
        envExpr (env, [], appVars (Var "map") ["inc", "zs"])), True),
    ( (env, [], Con "Cons" [hgy, mhmg]),
      (env, [], Con "Cons" [hgy, mhmg]), True),
    ( (env, [], parseExpr
      "case Cons (g y) (map g ys) of \
      \  Nil->[];\
      \  Cons y ys->Cons (h y) (map h ys);"),
      (env, [], Con "Cons" [hgy, mhmg]), True)
  ]
  where mhmg = app (Var "map") [Var "h", appVars (Var "map") ["g", "ys"]]
        hgy = App (Var "h") $ App (Var "g") (Var "y")

env :: Env
env = map (second parseExpr)
  [
  ("inc", "{n->Succ n}"),
  ("map",
    "{f->{xs->case xs of Nil->[];Cons y ys->Cons (f y) (map f ys);}}")
  ]


embTest :: TestTree
embTest = testGroup "emb ~~>" $
  map (\(x,y,v) ->
    testCase (x ++ " <| " ++ y) $
      parseExpr x <| parseExpr y @?= v)
  [
    ("x", "x", True),
    ("x", "y", True),
    ("f x", "f y", True),
    ("f a b", "f c d", True),
    ("f a a", "f b b", True),
    ("[1,2,3,x]", "[1,2,3,y]", True),
    --("Succ n", "{n->Succ n}", True),
    ("b", "a b", True),
    ("c b", "c (a b)", True),
    ("d b b", "d (a b) (a b)", True),
    ("a (c b)", "c b", False),
    ("a (c b)", "c (a b)", False),
    ("a (c b)", "a (a (a b))", False)
  ]


msgTest :: TestTree
msgTest = testGroup "msg ~~>" $
  map (\(x,y,v) ->
    testCase (x ++ " |><| " ++ y) $
      parseExpr x |><| parseExpr y @?= v)
  [
    ("x", "x", (Var "x", [], [])),
    ("x", "y", (Var "$x", [("$x", Var "x")], [("$x", Var "y")]))
    -- ("x", "y", True),
    -- ("f x", "f y", True),
    -- ("f a b", "f c d", True),
    -- ("f a a", "f b b", True),
    -- ("[1,2,3,x]", "[1,2,3,y]", True),
    -- --("Succ n", "{n->Succ n}", True),
    -- ("b", "a b", True),
    -- ("c b", "c (a b)", True),
    -- ("d b b", "d (a b) (a b)", True),
    -- ("a (c b)", "c b", False),
    -- ("a (c b)", "c (a b)", False),
    -- ("a (c b)", "a (a (a b))", False)
  ]

uniTest :: TestTree
uniTest = testGroup "uni ~~>" $
  map (\(x, y, s) ->
    testCase (x ++ " |~~| " ++ y) $
      parseExpr x |~~| parseExpr y @?= s)
  [
    ("x", "x", Just []),
    ("x", "y", Just [("x", Var "y")]),
    ("f g", "a b",
      Just [("f", parseExpr "a"), ("g", parseExpr "b")] ),
    ("(f g) (a b)", "x y",
      Just [("x", parseExpr "f g"), ("y", parseExpr "a b")] ),
    ("(f g) (a b)", "x x",
      Just [("x", parseExpr "f g"), ("x", parseExpr "a b")] )
  ]

uniTest' :: TestTree
uniTest' = testGroup "uni ~~>" $
  map (\(x, y, s) ->
    let xe = parseExpr x in
    let ye = parseExpr y in
    let (Just subst) = xe |~~| ye in
      testCase (x ++ " |~~| " ++ y ++ show subst) $
        substAlts subst xe @?= substAlts subst ye)
  [
    ("x", "x", Just []),
    ("x", "y", Just [("x", Var "y")]),
    ("f g", "a b",
      Just [("f", parseExpr "a"), ("g", parseExpr "b")] ),
    ("(f g) (a b)", "x y",
      Just [("x", parseExpr "f g"), ("y", parseExpr "a b")] ),
    ("(f g) (a b)", "x x",
      Just [("x", parseExpr "f g"), ("x", parseExpr "a b p")] ),
    ("Cons x xs", "Cons 2 Nil",
      Just [] ),
    ("Branch 2 t t", "Branch v x y", Just []),
    ("{x->x}", "{y->y}", Just [])

  ]

main :: IO ()
main = defaultMain $
  testGroup "Supercompiler: match, supercompile/eval"
    [
      testMatch,
      testMatch2,
      embTest,
      msgTest,
      uniTest,
      uniTest'
    ]
