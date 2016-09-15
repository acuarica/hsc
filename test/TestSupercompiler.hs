
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
  map (\(e, f, ar) -> testCase (e ++ "/" ++ ar) $
    eval (f (sp $ prelude ++ e)) @?= (eval . parseExpr) ar)
  [
  (mapinczs, Let "zs" (parse "[]"), "[]"),
  (mapinczs, Let "zs" (parse "[1,2,3,4,5]"), "[2,3,4,5,6]"),
  (mapinc, \e-> App e (parse "[]"), "[]"),
  (mapinc, \e-> App e (parse "[1,2,3,4,5]"), "[2,3,4,5,6]"),
  (mapincmapinczs, Let "zs" (parse "[]"), "[]"),
  (mapincmapinczs, Let "zs" (parse "[1,2,3,4,5]"), "[3,4,5,6,7]"),
  (mapincmapinc, \e-> App e (parse "[]"), "[]"),
  (mapincmapinc, \e-> App e (parse "[1,2,3,4,5]"), "[3,4,5,6,7]"),
  (appendasbs, letp "as" "[1,2,3]" . letp "bs" "[4,5]", "[1,2,3,4,5]"),
  (append, letp "as" "[]".letp "bs" "[]".letp "cs" "[]", "[]"),
  (append, letp "as" "[]".letp "bs" "[]".letp "cs" "[C, D]", "[C, D]"),
  (append, letp "as" "[A]".letp "bs" "[B]".letp "cs" "[C]", "[A, B, C]"),
  (append, letp "as" "[A]".letp "bs" "[B]".letp "cs" "[C, D]", "[A, B, C, D]")
    -- (revzs, Let "zs" (parse "[]"), "[]")
  ]
  where
    letp v val = Let v (parse val)
    s = newConf emptyEnv . parseExpr
    parse = parseExpr
    sp = supercompile . parse
    mapinczs = "map inc zs"
    mapinc = "map inc"
    mapincmapinczs = "map inc (map inc zs)"
    mapincmapinc = "let mimi={zs->map inc (map inc zs)} in mimi"
    appendasbs = "append as bs"
    append = "append (append as bs) cs"
    revzs = "rev vs"
    revAccum = "let reverse={rs->revAccum rs []} in reverse zs"
    prelude =
      "let id={a->a} in \
      \let app={p->{q->p q}} in \
      \let inc={n->Succ n} in \
      \let cp={a->case a of Zero->0;Succ aa->Succ (cp aa);} in \
      \let append={xs->{ys->case xs of Nil->ys;Cons z zs->Cons z (append zs ys);}} in \
      \let rev={rs-> case rs of Nil->Nil;Cons s ss->append (rev ss) [s];} in \
      \let revA={xs->{as->case xs of Nil->as;Cons y ys->revA ys (Cons y as);}}in \
      \let map={f->{xs->case xs of Nil->Nil;Cons y ys->Cons (f y)(map f ys);}}in \
      \let plus={n->{m->case n of Zero->m; Succ nn->plus nn (Succ m);}} in \
      \let mult={n->{m->case n of Zero->0; Succ nn->plus (mult nn m) m;}} in \
      \let len={xs->case xs of Nil->0; Cons y ys->Succ (len ys);} in "

main :: IO ()
main = defaultMain $ testGroup "Supercompiler " [testSupercompile]
