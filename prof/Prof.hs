
module Prof (prof) where

import System.CPUTime (getCPUTime)
import Control.DeepSeq (NFData(), deepseq)
import Control.Monad (replicateM)
import Text.Printf (printf)

import Expr (Expr())

prelude :: String
prelude =
  "let \
  \  id = {a->a} ;\
  \  app = {p->{q->p q}} ;\
  \  inc = {n->Succ n} ;\
  \  copyN = {n->case n of \
  \    Zero -> 0;\
  \    Succ n' -> Succ (copyN n');\
  \  };\
  \  append = {xs->{ys->case xs of \
  \    Nil -> ys;\
  \    Cons x xs' -> Cons x (append xs' ys);\
  \  }};\
  \  reverse' = {xs->case xs of \
  \    Nil -> Nil;\
  \    Cons x xs' -> append (reverse' xs') [x];\
  \  };\
  \  reverse = {xs-> \
  \    let reverseAccum = {xs->{as->case xs of \
  \      Nil -> as;\
  \      Cons y ys -> reverseAccum ys (Cons y as);\
  \    }} in reverseAccum xs [] }  ;\
  \  map = {f->{xs->case xs of \
  \    Nil->Nil;\
  \    Cons y ys->Cons (f y)(map f ys);\
  \  }};\
  \  plus = {n->{m->case n of \
  \    Zero->m;\
  \    Succ nn->plus nn (Succ m);\
  \  }};\
  \  mult = {n->{m->case n of \
  \    Zero->0;\
  \    Succ nn->plus (mult nn m) m;\
  \  }};\
  \  len = {xs->case xs of Nil->0; Cons y ys->Succ (len ys);} ; \
  \  head = {xs->case xs of Cons y ys -> y; } ; \
  \  tail = {xs->case xs of Cons y ys -> ys; } ; \
  \  inf = {n->Cons n (inf (Succ n))} ; \
  \  infA = Cons A inf in "

prelude1k   = (concat $ replicate     1 prelude) ++ "x"
prelude10k  = (concat $ replicate    10 prelude) ++ "x"
prelude100k = (concat $ replicate   100 prelude) ++ "x"
prelude1m   = (concat $ replicate  1000 prelude) ++ "x"
prelude10m  = (concat $ replicate 10000 prelude) ++ "x"

profComp :: NFData a => a -> IO Double
profComp a =
  do
    start <- getCPUTime
    end <- a `deepseq` getCPUTime
    let diff = (fromIntegral (end - start)) / (10^12)
    return (diff :: Double)

ts :: Double -> String
ts d = printf "%0.3f" d

profParser :: (String -> Expr) -> String -> String -> IO ()
profParser parseExpr msg s =
  do
    diffs <- replicateM 5 (profComp (replicate 10 $ parseExpr s))
    printf "%s: %s\n" msg (unwords $ map (ts) diffs)

prof :: (String -> Expr) -> IO ()
prof parseExpr =
  do
    profParser parseExpr "prelude1k  "   prelude1k
    profParser parseExpr "prelude10k "  prelude10k
    profParser parseExpr "prelude100k" prelude100k
    profParser parseExpr "prelude1m  "   prelude1m
    profParser parseExpr "prelude10m "  prelude10m
