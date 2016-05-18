
module Main where

import Util (doTests)
import Parser (parseExpr)
import Eval (newConf, emptyEnv, reduce)
import Splitter (split)

e="let inc={n->Succ n}\
\in let map={f->{xs->case xs of \
\  Nil->Nil;\
\  Cons y ys-> Cons (f y) (map f ys);}}\
\in map inc zs"

e'="let inc={n->Succ n}\
\in let map={f->{xs->case xs of \
\  Nil->Nil;\
\  Cons y ys-> Cons (f y) (map f ys);}}\
\in map inc"

main :: IO ()
main = do
  print $ reduceExpr e
  print $ map reduce $ split $ reduceExpr e'
  -- doTests (\(s, ss) -> (split (reduceExpr s), map toConf ss) ) [
  --   ("x", []),
  --   ("Cons x xs", ["x", "xs"]),
  --   ("{x->x}", ["x"]),
  --   (
  --   "let inc={n->Succ n}\
  --   \in let map={f->{xs->case xs of \
  --   \  Nil->Nil;\
  --   \  Cons y ys-> Cons (f y) (map f ys);}}\
  --   \in map inc zs", []),
  --   (
  --   "let inc={n->Succ n}\
  --   \in let map={f->{xs->case xs of \
  --   \  Nil->Nil;\
  --   \  Cons y ys-> Cons (f y) (map f ys);}}\
  --   \in map inc", [])
  --   ]
  where
    parse = parseExpr
    toConf = newConf emptyEnv . parse
    reduceExpr = reduce . toConf
