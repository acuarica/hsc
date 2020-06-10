
module Main (main) where

import Control.Arrow (second)

import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Expr (substAlts, alpha)
import Parser (parseExpr)
import Match ((|~~|), (<|), (|><|))

unificationTest :: TestTree
unificationTest = testGroup "unification" [
  go "x" "x" $ Just [],
  go "x" "y" $ Just [("x", "y")],
  go "x" "f x" Nothing,
  go "f g" "a b" $ Just [("f", "a"), ("g", "b")],
  go "(f g) (a b)" "x y" $ Just [("x", "f g"), ("y", "a b")],
  go "(f g) (a b)" "x x" $ Just [("f", "a"), ("g", "b"), ("x", "a b")],
  go "Cons x xs" "Cons 2 Nil" $ Just [("x", "2"), ("xs", "Nil")],
  go "Branch 2 t t" "Branch v x y" $ Just [("v", "2"), ("x", "y"), ("t", "y")],
  go "{x->x}" "{y->y}" $ Just [],
  go "plus n m" "plus n (Succ m)" Nothing,
  go "plus n m" "plus n' (Succ m')" $ Just [("n", "n'"), ("m", "Succ m'")] -- ,
    -- go "case n of Zero->Succ m;Succ n'->plus n' (Succ (Succ m));"
    --   "case a of Zero->m;Succ n'->plus n' (Succ m);" Nothing
  ]
  where
    go tx ty est =
        let (xe, ye) = (parseExpr tx, parseExpr ty) in
        let s = xe |~~| ye in
        let es = map (second parseExpr) <$> est in
        testGroup (tx ++ " |~~| " ++ ty ++ " == " ++ show s) $
          testCase "Expected" (xe |~~| ye @?= es) :
            case s of
              Nothing -> []
              Just s' -> [testCase "Subst" $
                alpha (substAlts s' xe) @?= alpha (substAlts s' ye)]

embTest :: TestTree
embTest = testGroup "emb ~~>" [
    go "x" "x" True,
    go "x" "y" True,
    go "f x" "f y" True,
    go "f a b" "f c d" True,
    go "f a a" "f b b" True,
    go "[1,2,3,x]" "[1,2,3,y]" True,
    --go "Succ n", "{n->Succ n}", True,
    go "b" "a b" True,
    go "c b" "c (a b)" True,
    go "d b b" "d (a b) (a b)" True,
    go "a (c b)" "c b" False,
    go "a (c b)" "c (a b)" False,
    go "a (c b)" "a (a (a b))" False,
    go "x" "Succ x" True,
    go "plus n m" "plus n (Succ m)" True,
    go "plus n (Succ m)" "plus n m" False,
    go "m" "plus n m" False,
    go "plus n m" "m" False,
    go "rev' zs []" "rev' $zs_ys (Cons $zs_y [])" True,
    go "rev' $zs_ys (Cons $zs_y [])" "rev' zs []" False
  ]
  where go x y v = testCase (x ++ " <| " ++ y) $ parseExpr x <| parseExpr y @?= v

msgTest :: TestTree
msgTest = testGroup "msg ~~>" [
    go "x" "x" ("x", [], []),
    go "x" "y" ("$x", [("$x", "x")], [("$x", "y")]),
    go "f x" "f y" ("$x", [("$x", "x")], [("$x", "y")]),
    go "plus n m" "plus n (Succ m)"
      ("plus n $0", [("$0", "m")], [("$0", "Succ m")]) -- ,
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
    -- go "case n of Zero->Succ m;Succ n'->plus n' (Succ (Succ m));"
    --   "case a of Zero->m;Succ n'->plus n' (Succ m);"
    --   ("case $0 of Zero->$1; Succ n'->plus n' (Succ $1);",
    --    [("$0", "n"), ("$1", "Succ m")],
    --    [("$0", "a"), ("$1", "m")])
  ]
  where
    go x y v = testCase (x ++ " |><| " ++ y ++ show v) $ domsg x y @?= pv v
    domsg x y = parseExpr x |><| parseExpr y
    pv (e, s, t) = (parseExpr e, map (second parseExpr) s,
                                 map (second parseExpr) t)

main :: IO ()
main = defaultMain $ testGroup "Match" [
  unificationTest,
  embTest,
  msgTest
  ]
