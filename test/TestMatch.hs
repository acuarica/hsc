
module Main where

import Util
import Expr
import Parser
import Eval
import Splitter
import Supercompile

e' = "let map={f->{xs->case xs of \
  \Nil->Nil;Cons y ys-> Cons (f y) (map f ys);}} in map {n->Succ n} zs"

mapinc = "let inc={n->Succ n}\
 \in let map={f->{xs->case xs of \
 \  Nil->Nil;\
 \  Cons y ys-> Cons (f y) (map f ys);}}\
 \in map inc zs"

mapmap = "let inc={n->Succ n}\
 \in let map={f->{xs->case xs of \
 \  Nil->Nil;\
 \  Cons y ys-> Cons (f y) (map f ys);}}\
 \in map inc (map inc zs)"

parse = parseExpr
sp = supercompile . parse

runmatch x y =
  match (newConf emptyEnv (parseExpr x)) (newConf emptyEnv (parseExpr y))

main :: IO ()
main = doTests (\(x,y,v)-> runmatch x y == v) [
  ("[1,2,3,x]", "[1,2,3,y]", True),
  ("let i={n->Succ n} in i a", "let i={n->Succ n} in i b", True)
  ]
