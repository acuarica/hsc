
module Main (main) where

import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Control.Arrow (second)

import Parser (parseExpr)
import Eval (newConf, emptyEnv, reduce)
import Splitter (split)

main :: IO ()
main = defaultMain $ testGroup "eval str ~~> expr" $
  map (\(s, ss) -> testCase s $ split (reduceExpr s) @?= map toConf ss)
  [
    ("x", []),
    ("Cons x xs", ["x", "xs"])
  ] ++
  map (\(s, ss) -> testCase (show s) $ split ((reduce.toConf') s) @?= map toConf' ss)
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
