
module Prof (prof) where

import Data.List (intercalate)
import System.CPUTime (getCPUTime)
import Control.DeepSeq (NFData(), deepseq)
import Control.Monad (replicateM)
import Text.Printf (printf)

import Expr (Expr(), Pat())

instance NFData Expr
instance NFData Pat

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

p :: Int -> String
p n = (concat $ replicate n prelude) ++ "x"

p1k   = p 1
p10k  = p 10
p100k = p 100
p1m   = p 1000
p10m  = p 10000

profComp :: NFData a => a -> IO Double
profComp a = do
  start <- getCPUTime
  end <- a `deepseq` getCPUTime
  return $ (fromIntegral (end - start)) / (10^12)

profParser :: (String -> Expr) -> (String, String) -> IO String
profParser parseExpr (msg, s) = do
  diffs <- replicateM 3 (profComp (parseExpr s))
  return $ printf "%s: %s\n" msg (unwords $ map (printf "%0.3f") diffs)

prof :: (String -> Expr) -> IO ()
prof parseExpr = do
  let ps = [
        ("1k", p1k),
        ("10k", p10k),
        ("100k", p100k),
        ("1m", p1m),
        ("10m", p10m)
        ]
  msgs <- mapM (profParser parseExpr) ps
  putStrLn $ intercalate "" msgs
