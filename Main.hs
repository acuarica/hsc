
module Main where

import Control.Arrow (second)

import System.Exit

import Expr
import Parser
import Eval
import Supercompile

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

inc = ("inc", "{n->Succ n}")
mp = ("map", "{f->{xs->case xs of Nil->[];Cons y ys->Cons (f y) (map f ys);}}")
env = map (second parseExpr) [inc, mp]
l = ([], [], envToLet env (appVars (Var "map") ["inc", "zs"]))
r = (env, [], appVars (Var "map") ["inc", "ys"])


main :: IO ()
main = do
  (print . gp . runMemo . parseExpr) mapincmapinczs
  (print . runMemo . parseExpr) mapincmapinczs
