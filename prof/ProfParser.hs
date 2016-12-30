
module Main (main) where

import System.CPUTime (getCPUTime)
import Control.DeepSeq (NFData(), deepseq)
import Control.Monad (replicateM_)
import Text.Printf (printf)

import Expr (Expr())
import qualified Parser (parseExpr)
import qualified FastParser (parseExpr)

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

prof :: NFData a => String -> a -> IO (Double, String)
prof msg a =
  do
    start <- getCPUTime
    end <- a `deepseq` getCPUTime
    let diff = (fromIntegral (end - start)) / (10^12)
    return (diff :: Double, printf "%s: %0.3f sec" msg (diff :: Double))

profParsers :: String -> String -> IO ()
profParsers msg s =
  putStrLn msg >>
  replicateM_ 5 (
    do
      (fp, mfp) <- prof "FastParser" (replicate 10 $ FastParser.parseExpr s)
      --(p, mp) <- prof "Parser" (replicate 10 $ Parser.parseExpr s)
      --printf "  %s / %s => Ratio: %0.3f\n" mfp mp (fp / p)
      print fp
    )

main :: IO ()
main =
  do
    profParsers   "prelude1k"   prelude1k
    profParsers  "prelude10k"  prelude10k
    profParsers "prelude100k" prelude100k
    profParsers   "prelude1m"   prelude1m
    profParsers  "prelude10m"  prelude10m
