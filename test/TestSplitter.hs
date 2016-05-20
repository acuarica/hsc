
module Main (main) where

import Test.HUnit (Counts, runTestTT, test, (~:), (~?=))
import Control.Arrow (second)

import Parser (parseExpr)
import Eval (newConf, emptyEnv, reduce)
import Splitter (split)

main :: IO Counts
main = runTestTT $ test $
  map (\(s, ss) -> "split1" ~: s ~: split (reduceExpr s) ~?= map toConf ss)
  [
    ("x", []),
    ("Cons x xs", ["x", "xs"])
  ] ++
  map (\(s, ss) -> "split2" ~: show s ~: split ((reduce.toConf') s) ~?= map toConf' ss)
  [
    ((env, [], "map inc zs"), [
      (env, [], "[]"),
      (env, [], "Cons (inc y) (map inc ys)")
      ])
  ]
  where
    env = map (second parseExpr) [
      ("inc", "{n->Succ n}"),
      ("map", "{f->{xs->case xs of Nil->Nil;Cons y ys-> Cons (f y) (map f ys);}}")
      ]
    toConf = newConf emptyEnv . parseExpr
    reduceExpr = reduce . toConf
    toConf' (env, stack, str) = (env, stack, parseExpr str)
