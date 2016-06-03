
module Main (main) where

import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Expr (Expr(..), Pat(Pat), con, app, zero, suc, cons, nil)
import Parser (parseExpr)
import Eval --(eval, whnf)
import Simplifier

inc = ("inc", "{n->Succ n}")
mp = ("map", "")

main :: IO ()
main = defaultMain $ testGroup "eval expr ~~> expr" $
  map (\(a, e) ->
    testCase "" $
      (toExpr . doSimp . toConf) a @?= parseExpr e)
  [
    -- (
    -- "let cat={xs->{ys->case xs of \
    -- \Nil->ys;Cons z zs->Cons z (cat zs ys);}} in \
    -- \let rev={rs->case rs of \
    -- \Nil->[];Cons s ss->cat (rev ss) (Cons s []);} in \
    -- \rev vs", "y"),
    -- (
    -- "let inc={n->Succ n}\
    -- \in let map={f->{xs-> case xs of \
    -- \  Nil->Nil;\
    -- \  Cons y ys -> Cons (f y) (map f ys) ; }}\
    -- \in Cons (inc y) (map inc ys)", "x"),
    (
    "let append={xs->{ys->case xs of \
    \  Nil->ys;\
    \  Cons z zs->Cons z (append zs ys);}} in \
    \append (append as bs) cs", "y")
  ]
  where
    toConf = newConf emptyEnv . parseExpr
    reduceExpr = reduce . toConf
