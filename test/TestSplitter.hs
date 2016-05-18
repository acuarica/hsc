
module Main where

import Control.Arrow (second)
import Test.HUnit

import Parser (parseExpr)
import Eval (newConf, emptyEnv, reduce)
import Splitter (split)

main :: IO Counts
main = runTestTT $ test $
  map testSplit1 [
    ("x", []),
    ("Cons x xs", ["x", "xs"])
  ] ++
  map testSplit2 [
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
    testSplit1 (s, ss) = "split1" ~: s ~: map toConf ss ~=? split (reduceExpr s)
    testSplit2 (s, ss) = "split2" ~: show s ~: map toConf' ss ~=? split ((reduce.toConf') s)
    parse = parseExpr
    toConf = newConf emptyEnv . parseExpr
    reduceExpr = reduce . toConf
    toConf' (env, stack, str) = (env, stack, parseExpr str)
