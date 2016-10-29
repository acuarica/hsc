
module Main (main) where

import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Control.Arrow (second)

import Expr (Expr(Var, Con, App), app, appVars, substAlts, alpha)
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
msgTest = testGroup "msg ~~>"
  [
    go "x" "x"
      (Var "x", [], []),
    go "x" "y"
      (Var "$x", [("$x", Var "x")], [("$x", Var "y")]),
    go "f x" "f y"
      (Var "$x", [("$x", Var "x")], [("$x", Var "y")])
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
  where
    go x y v = testCase (x ++ " |><| " ++ y) $ domsg x y @?= v
    domsg x y = parseExpr x |><| parseExpr y

unificationTest = testGroup "Unification tests" $
  let go tx ty est =
        let (xe, ye) = (parseExpr tx, parseExpr ty) in
        let s = xe |~~| ye in
        let es = map (second parseExpr) <$> est in
        testGroup (tx ++ " |~~| " ++ ty ++ " == " ++ show s) $
          testCase "Expected" (xe |~~| ye @?= es) :
            case s of
              Nothing -> []
              Just s' -> [testCase "Subst" $
                alpha (substAlts s' xe) @?= alpha (substAlts s' ye)]
          in
  [
    go "x" "x" $
      Just [],
    go "x" "y" $
      Just [("x", "y")],
    go "x" "f x"
      Nothing,
    go "f g" "a b" $
      Just [("f", "a"), ("g", "b")],
    go "(f g) (a b)" "x y" $
      Just [("x", "f g"), ("y", "a b")],
    go "(f g) (a b)" "x x" $
      Just [("f", "a"), ("g", "b"), ("x", "a b")],
    go "Cons x xs" "Cons 2 Nil" $
      Just [("x", "2"), ("xs", "Nil")],
    go "Branch 2 t t" "Branch v x y" $
      Just [("v", "2"), ("x", "y"), ("t", "y")],
    go "{x->x}" "{y->y}" $
      Just []
  ]

main :: IO ()
main = defaultMain $ testGroup "Match" [
    testMatch,
    testMatch2,
    embTest,
    unificationTest,
    msgTest
  ]
