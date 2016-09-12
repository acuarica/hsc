
module Main (main) where

import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Control.Arrow (second)

import Expr (Expr(..), appVars)
import Parser (parseExpr)
import Eval (Env, newConf, emptyEnv, eval)
import Supercompiler (supercompile)

testSupercompile :: TestTree
testSupercompile = testGroup "supercompile" $
  map (\(e, f, ar) ->
    testCase (e ++ "/" ++ ar ++ " ~~> " ++ show (sp e)) $
      eval (f (sp e)) @?= (eval . parseExpr) ar)
  [
    (mapinczs, Let "zs" (parse "[]"), "[]"),
    (mapinczs, Let "zs" (parse "[1,2,3,4,5]"), "[2,3,4,5,6]"),
--    (mapinc, \e-> App e (parse "[]"), "[]"),
--    (mapinc, \e-> App e (parse "[1,2,3,4,5]"), "[2,3,4,5,6]"),
    (mapincmapinczs, Let "zs" (parse "[]"), "[]"),
    (mapincmapinczs, Let "zs" (parse "[1,2,3,4,5]"), "[3,4,5,6,7]"),
--    (mapincmapinc, \e-> App e (parse "[]"), "[]"),
--    (mapincmapinc, \e-> App e (parse "[1,2,3,4,5]"), "[3,4,5,6,7]"),
--    (appendasbs,
--      Let "as" (parse "[1,2,3]") .
--        Let "bs" (parse "[4,5]"), "[1,2,3,4,5]"),
    (append, Let "as" (parse "[A]") . Let "bs" (parse "[B]") .
          Let "cs" (parse "[C]"),
       "[A, B, C]"),
   (append, Let "as" (parse "[A]") . Let "bs" (parse "[B]") .
         Let "cs" (parse "[C, D]"),
      "[A, B, C, D]")
    -- (revzs, Let "zs" (parse "[]"), "[]")
  ]
  where
    s = newConf emptyEnv . parseExpr
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
    mapincmapinc =
      "let inc={n->Succ n}\
      \in let map={f->{xs->case xs of \
      \  Nil->Nil;\
      \  Cons y ys-> Cons (f y) (map f ys);}}\
      \in let mimi={zs->map inc (map inc zs)} \
      \in mimi"
    appendasbs =
     "let append={xs->{ys->case xs of \
      \  Nil->ys;\
      \  Cons z zs -> Cons z (append zs ys) ; }}\
      \in append as bs"
    append =
     "let append={xs->{ys->case xs of \
      \  Nil->ys;\
      \  Cons z zs -> Cons z (append zs ys) ; }}\
      \in append (append as bs) cs"
    revzs =
      "let cat={xs->{ys->case xs of\
      \  Nil->ys; Cons z zs->Cons z (cat zs ys); }}\
      \in let rev={rs->case rs of\
      \  Nil->Nil; Cons s ss->cat (rev ss) [s]; }\
      \in rev vs"
    revAccum =
      "let revAccum={xs->{as->case xs of \
      \  Nil -> as;\
      \  Cons y ys -> revAccum ys (Cons y as); }}\
      \in let reverse={rs->revAccum rs []}\
      \in reverse zs"

env :: Env
env = map (second parseExpr)
  [
  ("inc", "{n->Succ n}"),
  ("map", "{f->{xs->case xs of Nil->[];Cons y ys->Cons (f y) (map f ys);}}")
  ]

main :: IO ()
main = defaultMain $ testGroup "Supercompiler " [testSupercompile]
