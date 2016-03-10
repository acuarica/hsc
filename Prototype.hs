module Main where

import Data.List (nub)
import Data.Maybe (mapMaybe)
import Debug.Trace (trace)

type Id = String

data Exp = Var Id
         | K Id
         | App Exp Exp
  deriving Eq

instance Show Exp where
  show (Var x) = x
  show (K x) = x
  show (App e1 e2 @ (App _ _)) = show e1 ++ " (" ++ show e2 ++ ")"
  show (App e1 e2) = show e1 ++ " " ++ show e2

data Rule = Rule Exp Exp

instance Show Rule where
  show (Rule e1 e2) = show e1 ++ " --> " ++ show e2

type Subst = [(String, Exp)]

-- apply a substitution
subst :: Exp -> Subst -> Exp
subst (Var x) s = 
  case lookup x s of
    Just e -> e
    Nothing -> Var x
subst (App e1 e2) s = App (subst e1 s) (subst e2 s)
subst (K x) s = K x

-- cost function for expressions. this is just a hack for now
-- it's very inaccurate since it assumes strict evaluation, not lazy.
cost :: Exp -> Integer
cost (Var x) = 1
cost (K x) = 1
cost (App (App (K ".") f) g) = 1
cost (App (App (K "map") f) xs) = cost f + cost xs + complexity f * listLen xs + 2
cost (App (App (K "++") xs) ys) = cost xs + cost ys + listLen xs + 2
cost (App f x) = cost f + cost x + complexity f + 1

-- estimate of function complexity
complexity :: Exp -> Integer
complexity (K x) = 1
complexity (App (App (K ".") f) g) = complexity f + complexity g
complexity _ = 10

-- estimate of list length
listLen :: Exp -> Integer
listLen (K "[]") = 0
listLen (App (App (K "map") f) xs) = listLen xs
listLen (App (K "tail") xs) = (listLen xs - 1) `max` 0
listLen (App (App (K ":") x) xs) = listLen xs + 1
listLen (App (App (K "++") xs) ys) = listLen xs + listLen ys
listLen _ = 100

type Pat = Exp

-- match a pattern to an expression, returning a substitution
match :: Pat -> Exp -> Maybe Subst
match (K k1) (K k2) | k1 == k2  = Just []
match (Var x) e                 = Just [(x, e)]
match (App e1 e2) (App e1' e2') = do
  s1 <- match e1 e1'
  s2 <- match e2 e2'
  return $ s1 ++ s2
match _ _                       = Nothing

-- apply a single rule to an expression. might fail.
rewrite1 :: Rule -> Exp -> Maybe Exp
rewrite1 (Rule p q) e = do
  s <- match p e 
  -- trace ("matched <" ++ show p ++ ">" ++ " ~~ <" ++ show e ++ "> :: " ++ show s) return ()
  return $ subst q s

-- Apply rewrite1 to the expression for each rule in the list
rewriteLevel :: [Rule] -> Exp -> [Exp]
rewriteLevel rs e = mapMaybe ((flip rewrite1) e) rs

-- Rewrite an expression, applying rules bottom up, returning all
-- possible rewrites, including the identity rewrite
-- If a rewrite enables another rewrite, the second rewrite is
-- NOT performed
rewriteEverywhereOnce :: [Rule] -> Exp -> [Exp]
rewriteEverywhereOnce rules e @ (App e1 e2) = nub $ es ++ es'
  where
    es  = e : rewriteLevel rules e
    es' = [App e1' e2' | e1' <- r e1, e2' <- r e2]
    r  = rewriteEverywhereOnce rules
rewriteEverywhereOnce rules e = e : rewriteLevel rules e

-- Rewrite an expression, applying rules bottom up, returning all
-- possible rewrites, including the identity rewrite.
-- Rewrites are performed until a fixed point is reached.
-- FIXME: this is really expensive and should be implemented with
-- some sort of dynamic programming algorithm. Probably the fixpoint
-- should be within rewriteLevel not here and rewriteLevel
-- should annotate with costs.
rewrite :: [Rule] -> Exp -> [Exp]
rewrite rules e = fix es
  where
    es = nub $ rewriteEverywhereOnce rules e
    fix es =
      -- apply the rewrite again to es
      -- if no new rewrites are added, stop, otherwise go again
      do 
        let es' = nub $ concatMap (rewriteEverywhereOnce rules) es
        if length es == length es'
          then es'
          else fix es'

-- Examples

-- Constructors
kMap = K "map"
kHead = K "head"
kTail = K "tail"
kNil = K "[]"
kCons = K ":"
kCompose = K "."
kId = K "id"
kPlus = K "+"
kAppend = K "++"

appMap e1 e2 = (App (App kMap e1) e2)
appHead e = (App kHead e)
appTail e = (App kTail e)
appCompose e1 e2 = (App (App kCompose e1) e2)
appCons e1 e2 = (App (App kCons e1) e2)
appId e = (App kId e)
appPlus e1 e2 = (App (App kPlus e1) e2)
appAppend e1 e2 = (App (App kAppend e1) e2)

-- numbers
k0 = K "0"
k1 = K "1"
k2 = K "2"
k3 = K "3"
k4 = K "4"

-- Rules
rules :: [Rule]
rules = [
    -- + commutes
    Rule (appPlus (Var "X") (Var "Y"))
         (appPlus (Var "Y") (Var "X"))
    -- + assoc
  , Rule (appPlus (appPlus (Var "X") (Var "Y")) (Var "Z"))
         (appPlus (Var "X") (appPlus (Var "Y") (Var "Z")))
    -- x + 0 == x
  , Rule (appPlus (Var "X") k0)
         (Var "X")
    -- encoding constant folding
  , Rule (appPlus k1 k1) k2
  , Rule (appPlus k1 k2) k3
  , Rule (appPlus k1 k3) k4
  , Rule (appPlus k2 k2) k4
  , Rule (appPlus k2 (Var "N")) (appPlus k1 (appPlus k1 (Var "N")))
  , Rule (appPlus k3 (Var "N")) (appPlus k1 (appPlus k2 (Var "N")))
   
    -- id x == x
  , Rule (appId (Var "X"))
       (Var "X")
    -- map id == id
  , Rule (App kMap kId)
       kId
    -- map f (xs ++ ys) == map f xs ++ map f ys
  , Rule (appMap (Var "F") (appAppend (Var "XS") (Var "YS")))
       (appAppend (appMap (Var "F") (Var "XS")) (appMap (Var "F") (Var "YS")))
    -- map f (map g xs) == map (f . g) xs
  , Rule (appMap (Var "F") (appMap (Var "G") (Var "XS")))
       (appMap (appCompose (Var "F") (Var "G")) (Var "XS"))
    -- map f (tail xs) == tail (map f xs)
  , Rule (appMap (Var "F") (appTail (Var "XS")))
       (appTail (appMap (Var "F") (Var "XS")))
    -- map f [] == []
  , Rule (appMap (Var "F") kNil)
       kNil
    -- tail (map f xs) == map f (tail xs)
  , Rule (appTail (appMap (Var "F") (Var "XS")))
       (appMap (Var "F") (appTail (Var "XS")))
    -- head (map f xs) == f (head xs)
  , Rule (appHead (appMap (Var "F") (Var "XS")))
       (App (Var "F") (appHead (Var "XS")))
    -- head (x:xs) = x
  , Rule (appHead (appCons (Var "X") (Var "XS")))
       (Var "X")
    -- tail (x:xs) = xs
  , Rule (appTail (appCons (Var "X") (Var "XS")))
       (Var "XS")
    -- (f . g) x == f (g x)
  , Rule (App (appCompose (Var "F") (Var "G")) (Var "X"))
       (App (Var "F") (App (Var "G") (Var "X")))
       ]

-- Example expressions
ex0 :: Exp
ex0 = appMap (Var "f") (appMap (Var "g") (Var "xs"))

ex1 :: Exp
ex1 = appTail (appMap (Var "f") (appMap (Var "g") (Var "xs")))

ex2 :: Exp
ex2 = appHead (appMap (Var "f") (appMap (Var "g") (Var "xs")))

ex3 :: Exp
ex3 = appMap kId (Var "xs")

ex4 :: Exp
ex4 = appPlus k1 k2

ex5 :: Exp
ex5 = appPlus k0 k2

ex6 :: Exp
ex6 = appPlus (Var "x") (appPlus k0 (Var "y"))

ex7 :: Exp
ex7 = appMap (Var "f") (appMap (Var "g") (appAppend (Var "xs") kNil))

main = return ()
