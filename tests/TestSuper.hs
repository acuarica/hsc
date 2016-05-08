
module Main where

import Util
import Expr
import Parser
import Super
import Pretty

doSuper :: (String, Expr, State) -> (String, State, State)
doSuper (s, expr, expstate) = (s, expstate, reduce (newstate expr))

doParse :: (String, State) -> (String, Expr, State)
doParse (e, expstate) = (show (parseExpr e), parseExpr e, expstate)

main :: IO ()
main = doTests (doSuper . doParse) [
    ("x", ([], [], Var "x" False)),
    ("True", ([], [], Con "True" [])),
    ("Succ Zero", ([], [], Con "Succ" [Con "Zero" []])),
    ("{a->a}", ([], [], Lam "a" (Var "a" False))),
    ("{a->a} A", ([], [], Con "A" [])),
    ("{f->{x->f x}} Succ Zero", ([], [], Con "A" [])),
    ("{f->{x->f x}} {a->a} A", ([], [], Con "A" []))
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
