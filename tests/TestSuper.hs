
module Main where

import Util
import Expr
import Parser
import Super
import Pretty

doSuper :: (String, Expr, State) -> (String, State, State)
doSuper (s, expr, expstate) = (s, step (newstate expr), expstate)

doParse :: (String, State) -> (String, Expr, State)
doParse (e, expstate) = (show (parseExpr e), parseExpr e, expstate)

main :: IO ()
main = doTests (doSuper . doParse) [
    ("x", ([], [], [(Var "x" False, Push)])),
    ("True", ([], [], [(Var "x" False, Push)]))
    -- (
    -- "let inc={n->Succ n}\
    -- \in let map={f->{xs->case xs of \
    -- \  Nil->Nil;\
    -- \  Cons y ys->Cons (f y) (map f ys);}}\
    -- \in map inc", "A")

    -- (
    -- "let $mapinc={\\$xs-> case $xs of {\
    -- \  Nil->Nil;\
    -- \  Cons $y $ys -> Cons (Succ $y) ($mapinc $ys) ; }}\
    -- \in $mapinc", "A"),
  ]
