
module Main where

import Control.Arrow (second)

import System.Exit

import Expr
import Parser
import Eval
import Supercompiler

--fileText <- readFile "Setup.hs"
--print fileText
--print $ fromParseResult (parseFileContents fileText)
--print $ aform $ parseExpr "f (g (h x))"
--print $ aform $ parseExpr "f (g x) (g (h x)) (h y)"
--print $ aform $ parseExpr "f (g x) (g (h x)) (h y)"
--putStrLn "To implement ..." >> exitFailure
--mapM_ (print . supercompile . parseExpr) [
  --mapM_ (print . (\(c,(n,h,p))->p) . runMemo . parseExpr) [
    -- "x",
    -- "Tree",
    -- "Succ 3",
    -- "Succ n",
    -- "f x",
    --"case n of Zero->A B; Succ nn->C D E;"

    --e

  -- "let map={f->{xs->case xs of \
  -- \  Nil->Nil; \
  -- \  Cons y ys-> Cons (f y) (map f ys);}} \
  -- \in map g (map f ys)"


  --
  -- ( root =
  -- "let $mapinc={\\$xs-> case $xs of {\
  -- \  Nil->Nil;\
  -- \  Cons $y $ys -> Cons (Succ $y) ($mapinc $ys) ; }}\
  -- \in $mapinc", "A"),
--    ("x", usevar "x")
--    ]

mapinc = "let inc={n->Succ n}\
 \in let map={f->{xs->case xs of \
 \  Nil->Nil;\
 \  Cons y ys-> Cons (f y) (map f ys);}}\
 \in map inc "

mapmap = "let inc={n->Succ n}\
 \in let map={f->{xs->case xs of \
 \  Nil->Nil;\
 \  Cons y ys-> Cons (f y) (map f ys);}}\
 \in map inc (map inc zs)"

mapincmapinczs = "let inc={n->Succ n}\
 \in let map={f->{xs->case xs of \
 \  Nil->Nil;\
 \  Cons y ys-> Cons (f y) (map f ys);}}\
 \in map inc (map inc zs)"

append =
 "let append={xs->{ys->case xs of \
  \  Nil->ys;\
  \  Cons z zs -> Cons z (append zs ys) ; }}\
  \in append (append as bs) cs"

rev =
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


inc = ("inc", "{n->Succ n}")
mp = ("map", "{f->{xs->case xs of Nil->[];Cons y ys->Cons (f y) (map f ys);}}")
env = map (second parseExpr) [inc, mp]
l = ([], [], envToLet env (appVars (Var "map") ["inc", "zs"]))
r = (env, [], appVars (Var "map") ["inc", "ys"])


main :: IO ()
main = do
--  putStrLn rev
  (print . gp . runMemo . parseExpr) rev
  (print . runMemo . parseExpr) rev
