
module Main where

import Util
import Expr
import Parser
import Super
import Pretty

-- doSuper :: (String, Expr, Expr) -> (String, Expr, Expr)
-- doSuper (s, expr, expstate) = (s, supercompile expr, supercompile expr)
--
-- doParse :: (String, Expr) -> (String, Expr, Expr)
-- doParse (e, expstate) = (show (parseExpr e), parseExpr e, expstate)

s = "let inc={n->Succ n} in let map={f->{xs->case xs of   Nil->Nil; Cons y ys-> Cons (f y) (map f ys);}} in map inc ys"

e = parseExpr "let inc={n->Succ n}\
\ in let map={f->{xs->case xs of \
\  Nil->Nil; Cons y ys-> Cons (f y) (map f ys);}}\
\in map inc ys"

s' ="let inc={n->Succ n}\
\ in let map={f->{xs->case xs of \
\  Nil->Nil; \
\  Cons y ys-> Cons (f y) (map f ys);}}\
\in map inc [1,2,3,4,5,6,7]"

e' = subst ("f", (Var "lala")) e

doe = showEnv . selH . tohist . flatten

main :: IO ()
main = -- putStrLn $ doe e
  --doTests (doSuper . doParse) [
    mapM_ (putStrLn . showState . reduce . newstate . parseExpr) [
    --("x", Var "x" False),
    --("True", true),
    --("Succ Zero", Con "Succ" [zero]),
    -- ("let x=Zero in Succ x", Con "Succ" [zero]),
    -- ("{a->a}", Lam "a" (usevar "a")),
    -- ("{a->a} A", Con "A" []),
    --("{f->{x->f x}} Succ Zero", Con "Succ" [Con "Zero" []]),
    --("{f->{x->f x}} {a->a} A", Con "A" [])--,
    -- (
    -- "let cp={n->case n of Zero->Zero; Succ nn->Succ (cp nn);}\
    -- \in cp 5", Con "A" [])
    --s'

    "let cat={xs->{ys->case xs of\
    \  Nil->ys; Cons z zs->Cons z (cat zs ys); }}\
    \in let rev={rs->case rs of\
    \  Nil->Nil; Cons s ss->cat (rev ss) [s]; }\
    \in rev [A,B]"

    --"Cons (Succ 1) [Succ 2, Succ (Succ 3),4,A]"

    --
    -- ( root =
    -- "let $mapinc={\\$xs-> case $xs of {\
    -- \  Nil->Nil;\
    -- \  Cons $y $ys -> Cons (Succ $y) ($mapinc $ys) ; }}\
    -- \in $mapinc", "A"),
--    ("x", usevar "x")
  ]
