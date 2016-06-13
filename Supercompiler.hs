{-# LANGUAGE FlexibleInstances #-}

module Supercompiler where

import Data.List (intercalate)
import Data.Maybe (isNothing, fromJust)
import Control.Exception (assert)
import Control.Monad.State (State, state, runState)

import Expr (Expr(..), Var, Pat(Pat), app, appVars, isVar, isEmptyCon, freeVars)
import Eval (Conf, Env, StackFrame(..),
  newConf, emptyEnv, toExpr, nf, reduce, put)
import Splitter (split, combine)
import Simplifier
import Match

import Debug.Trace

supercompile :: Expr -> Expr
supercompile = gp . runMemo

gp m@((_, _, expr0), (next, hist, prom)) = fromMemo prom expr0

runMemo expr = let s = memo "$root" (newConf emptyEnv expr)
  in runState s (0, [], [])

fromMemo :: [(Var, [Var], Conf)] -> Expr -> Expr
fromMemo [] expr = expr
fromMemo ((var, fv, valconf):env) expr =
    Let (var) (toLambda fv (toExpr valconf)) (fromMemo env expr)

add :: Var -> Expr -> Conf -> Conf
add var valexpr (env, stack, expr) = (put var valexpr env, stack, expr)

memo :: Var -> Conf -> Memo Conf
memo parentVar conf@(env, stack, expr) =
  --traceShow expr $
  if null stack && isVar expr then return conf else
  if null stack && isEmptyCon expr then return conf else
  do
  next <- getNext
  if next > 7 then return conf else
    do
    ii <- isin conf match
    if isNothing ii
      then do
        (v, fv) <- rec parentVar conf

        -- let rconf'' = reduce conf
        -- let rconf' = freduce (map (Var . (++) "$m_" . show) [1..10]) rconf''
        -- let rconf = reduce $ simp rconf'
        let rconf = doSimp conf

        let sps = split rconf
        --recProc conf sps
        splits <- mapM (memo v) sps
        let r@(env', stack', expr') = combine rconf splits
        promise v (fvs rconf) r
        return (env', stack', appVars (Var v) fv)
      else do
        let (var, _) = fromJust ii
        return (env, stack, appVars (Var var) (fvs conf))
  where fvs = freeVars . envExpr

type Hist = [(Var, Var, [Var], Conf)]

type Prom = [(Var, [Var], Conf)]

type Memo a = State (Int, Hist, Prom) a

rec :: Var -> Conf -> Memo (Var, [Var])
rec parentVar conf = state $ \(next, hist, prom) ->
  let var = "$v_" ++ show next in
  let fv = (freeVars . envExpr) conf in
    ((var, fv), (next+1, (parentVar, var, fv, conf):hist, prom))

promise :: Var -> [Var] -> Conf -> Memo ()
promise var fv conf = state $ \(next, hist, prom) ->
  ((), (next, hist, (var, fv, conf):prom))

isin :: Conf -> Match -> Memo (Maybe (Var, [Var]))
isin conf m = state $ \(next, hist, prom) ->
  (lookupMatch m hist conf, (next, hist, prom))

getNext :: Memo Int
getNext = state $ \(next, hist, prom) -> (next, (next, hist, prom))

lookupMatch :: Match -> Hist -> Conf -> Maybe (Var, [Var])
lookupMatch _ [] _ = Nothing
lookupMatch m ((parentVar, var, vars, conf'):hist) conf = if conf `m` conf'
  then Just (var, vars)
  else lookupMatch m hist conf

instance {-# OVERLAPPING #-} Show Hist where
  show hist = "" ++
    intercalate "\n" (map ((++) "  " . show) hist)

instance {-# OVERLAPPING #-} Show (Var, Var, [Var], Conf) where
  show (parentVar, var, args, expr) = parentVar ++ "->" ++ show (var, args, expr)

instance {-# OVERLAPPING #-} Show (Var, [Var], Conf) where
  show (var, args, expr) = var ++ "(" ++ unwords args ++ ") ~> " ++ show expr

instance {-# OVERLAPPING #-} Show (Var, Conf) where
  show (var, expr) = var ++ " ~> " ++ show expr

instance {-# OVERLAPPING #-} Show a => Show (a, (Int, Hist, Prom)) where
  show (val, (next, hist, prom)) =
    "Value: " ++ show val ++ "\n" ++
    "Next: " ++ show next ++ "\n" ++
    "Hist: \n" ++ show hist ++ "\n" ++
    "Prom: \n" ++ show prom
