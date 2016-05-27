
module Main (main) where

import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Control.Arrow (second)

import Expr (Expr(..), appVars)
import Parser (parseExpr)
import Eval (newConf, emptyEnv, eval)
import Supercompiler (envToLet, match, supercompile)

main :: IO ()
main = defaultMain $ testGroup "Supercompiler: match, supercompile/eval"
  [testGroup "match1 ~~>" $
  map testMatch1 [
    ("x", "y", True),
    ("f x", "f y", True),
    ("f a b", "f c d", True),
    ("f a a", "f b b", True),
    ("[1,2,3,x]", "[1,2,3,y]", True),
    ("{n->Succ n}", "Succ n", True),
    ("{n->Succ n}", "Succ m", True),
    ("Succ n", "{n->Succ n}", True),
    ("let inc={n->Succ n} in inc x", "let inc={n->Succ n} in inc y", True),
    ("let i={n->Succ n} in i a", "let i={n->Succ n} in i b", True),
    ("let i={n->Succ n} in i", "let i={n->Succ n} in i", True),
    ("let i={n->Succ n} in i", "{n->Succ n}", True),
    ("let i={n->Succ n} in i", "Succ n", True),
    ("let i={n->Succ n} in i",
      "let i={n->Succ n} in let id={x->x} in i m", True)
  ], testGroup "match2 ~~>" $
  map testMatch2 [
    ( (env, [], appVars (Var "map") ["inc", "zs"]),
      (env, [], appVars (Var "map") ["inc", "ys"]), True),
    ( ([], [], envToLet env (appVars (Var "map") ["inc", "zs"])),
      (env, [], appVars (Var "map") ["inc", "ys"]), True),
    ( (env, [], appVars (Var "map") ["inc", "ys"]),
      ([], [], envToLet env (appVars (Var "map") ["inc", "zs"])), True)
  ], testGroup "supercompile" $
  map testSupercompile [
    (mapinczs, Let "zs" (parse "[]"), "[]"),
    (mapinczs, Let "zs" (parse "[1,2,3,4,5]"), "[2,3,4,5,6]"),
    (mapinc, \e-> App e (parse "[]"), "[]"),
    (mapinc, \e-> App e (parse "[1,2,3,4,5]"), "[2,3,4,5,6]"),
    (mapincmapinczs, Let "zs" (parse "[]"), "[]"),
    (mapincmapinczs, Let "zs" (parse "[1,2,3,4,5]"), "[3,4,5,6,7]"),
    (mapincmapinc, \e-> App e (parse "[]"), "[]"),
    (mapincmapinc, \e-> App e (parse "[1,2,3,4,5]"), "[3,4,5,6,7]")
  ]
  ]
  where
    inc = ("inc", "{n->Succ n}")
    mp = ("map", "{f->{xs->case xs of Nil->[];Cons y ys->Cons (f y) (map f ys);}}")
    env = map (second parseExpr) [inc, mp]

    testMatch1 (x,y,v) = testCase (x ++ " =~= " ++ y) $ match (s x) (s y) @?= v
    testMatch2 (l,r,v) = testCase (show l ++ " =~= " ++ show "") $ l `match` r @?= v
    s = newConf emptyEnv . parseExpr
    testSupercompile (e, f, ar) = testCase (e ++ "/" ++ ar) $ eval (f (sp e)) @?= (eval . parseExpr) ar
    parse = parseExpr
    sp = supercompile . parse
    mapinczs =
      "let inc={n->Succ n}\
      \in let map={f->{xs->case xs of \
      \  Nil->Nil;\
      \  Cons y ys-> Cons (f y) (map f ys);}}\
      \in map inc zs"
    mapinc =
      "let inc={n->Succ n}\
      \in let map={f->{xs->case xs of \
      \  Nil->Nil;\
      \  Cons y ys-> Cons (f y) (map f ys);}}\
      \in map inc"
    mapincmapinczs =
      "let inc={n->Succ n}\
      \in let map={f->{xs->case xs of \
      \  Nil->Nil;\
      \  Cons y ys-> Cons (f y) (map f ys);}}\
      \in map inc (map inc zs)"
    mapincmapinc =
      "let inc={n->Succ n}\
      \in let map={f->{xs->case xs of \
      \  Nil->Nil;\
      \  Cons y ys-> Cons (f y) (map f ys);}}\
      \in let mimi={zs->map inc (map inc zs)} \
      \in mimi"





      --
      -- mapinczs = "let inc={n->Succ n}\
      --  \in let map={f->{xs->case xs of \
      --  \  Nil->Nil;\
      --  \  Cons y ys-> Cons (f y) (map f ys);}}\
      --  \in map inc zs"
      --
      -- mapinc = "let inc={n->Succ n}\
      --  \in let map={f->{xs->case xs of \
      --  \  Nil->Nil;\
      --  \  Cons y ys-> Cons (f y) (map f ys);}}\
      --  \in map inc "
      --
      -- mapmap = "let inc={n->Succ n}\
      --  \in let map={f->{xs->case xs of \
      --  \  Nil->Nil;\
      --  \  Cons y ys-> Cons (f y) (map f ys);}}\
      --  \in map inc (map inc zs)"
      --
      -- mapincmapinczs = "let inc={n->Succ n}\
      --  \in let map={f->{xs->case xs of \
      --  \  Nil->Nil;\
      --  \  Cons y ys-> Cons (f y) (map f ys);}}\
      --  \in map inc (map inc zs)"
      --
      -- append =
      --  "let append={xs->{ys->case xs of \
      --   \  Nil->ys;\
      --   \  Cons z zs -> Cons z (append zs ys) ; }}\
      --   \in append (append as bs) cs"
      --
      -- rev =
      --   "let cat={xs->{ys->case xs of\
      --   \  Nil->ys; Cons z zs->Cons z (cat zs ys); }}\
      --   \in let rev={rs->case rs of\
      --   \  Nil->Nil; Cons s ss->cat (rev ss) [s]; }\
      --   \in rev vs"
      --
      -- revAccum =
      --   "let revAccum={xs->{as->case xs of \
      --   \  Nil -> as;\
      --   \  Cons y ys -> revAccum ys (Cons y as); }}\
      --   \in let reverse={rs->revAccum rs []}\
      --   \in reverse zs"
      --
      --
      -- inc = ("inc", "{n->Succ n}")
      -- mp = ("map", "{f->{xs->case xs of Nil->[];Cons y ys->Cons (f y) (map f ys);}}")
      -- env = map (second parseExpr) [inc, mp]
      -- l = ([], [], envToLet env (appVars (Var "map") ["inc", "zs"]))
      -- r = (env, [], appVars (Var "map") ["inc", "ys"])
      --  putStrLn rev
      --  (print . gp . runMemo . parseExpr) append
        --(print . runMemo . parseExpr) rev
