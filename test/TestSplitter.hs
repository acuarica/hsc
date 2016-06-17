
module Main (main) where

import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Control.Arrow (second)

import Parser (parseExpr)
import Eval (newConf, emptyEnv, reduce)
import Splitter (split)

splitTest :: TestTree
splitTest = testGroup "Splitter test" $
  map (\(s, ss) ->
    testCase (s ++ " |< " ++ show ss) $
      split (reduce . toConf $ s) @?= map toConf ss)
  [
    ("x", []),
    ("Cons x xs", ["x", "xs"]),
    ("case vs of Nil->[];Cons s ss->cat (rev ss) (Cons s []);",
      ["[]", "cat (rev ss) (Cons s [])"])
  ]
  where toConf = newConf emptyEnv . parseExpr

splitConfTest :: TestTree
splitConfTest = testGroup "Splitter w/Conf test" $
  map (\(s@(_, _, exprText), ss) ->
    testCase (exprText ++ " |< " ++ show (map (\(_,_,a)->a) ss)) $
      split (reduce . toConf' $ s) @?= map toConf' ss)
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
    toConf' (env, stack, str) = (env, stack, parseExpr str)

main :: IO ()
main = defaultMain $ testGroup "Spliiter test" [splitTest, splitConfTest]
