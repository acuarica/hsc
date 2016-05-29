{-# LANGUAGE FlexibleInstances #-}

module Supercompiler where

import Data.List (intercalate)
import Data.Maybe (isNothing, fromJust)
import Control.Exception (assert)
import Control.Monad.State (State, state, runState)

import Expr (Expr(..), Var, Pat(Pat), app, appVars, isVar, isEmptyCon, freeVars)
import Eval (Conf, Env, StackFrame(..), newConf, emptyEnv, toExpr, nf, reduce, put, step)
import Splitter (split, combine)

import Debug.Trace

supercompile :: Expr -> Expr
supercompile = gp . runMemo

gp m@((_, _, expr0), (next, hist, prom)) = fromMemo prom expr0

runMemo expr = let s = memo (newConf emptyEnv expr)
  in runState s (0, [], [])

fromMemo :: [(Var, [Var], Conf)] -> Expr -> Expr
fromMemo [] expr = expr
fromMemo ((var, fv, valconf):env) expr =
    Let (var) (toLambda fv (toExpr valconf)) (fromMemo env expr)

toLambda :: [Var] -> Expr -> Expr
toLambda [] expr = expr
toLambda (v:vs) expr = Lam v (toLambda vs expr)

envToLet :: Env -> Expr -> Expr
envToLet [] expr = expr
envToLet ((var, valexpr):env) expr = Let var valexpr (envToLet env expr)

add :: Var -> Expr -> Conf -> Conf
add var valexpr (env, stack, expr) = (put var valexpr env, stack, expr)

freduce :: [Expr] -> Conf -> Conf
freduce args conf =
  let (env, stack, expr) = reduce conf in
  case expr of
    Lam var lamexpr ->
      assert (null stack)
        (freduce (tail args) (env, [Arg (head args)], Lam var lamexpr))
    _ -> (env, stack, expr)

simp :: Conf -> Conf
simp conf@(env, stack, expr) = case expr of
  Var var -> case stack of
    Alts alts:Alts alts':stack' ->
      (env, stack', Case (Var var) (map (al alts') alts))
    _ -> conf
  _ -> conf
  where al als (Pat tag vars, altexpr) = (Pat tag vars, Case altexpr als)

memoStep :: Conf -> Memo Conf
memoStep conf@(env, stack, expr) =
--  traceShow (stack, expr) $
  ---traceShow (toExpr conf) $
  if null stack && isVar expr then return conf else
  do
  next <- getNext
--  if next > 100 then return conf else
  do
    ii <- isin conf textualMatch
    --ii <- isin conf match
    if isNothing ii
      then do
        (v, fv) <- rec conf
        case step conf of
          Nothing -> do --return conf
            let rconf = conf
            --splits <- trace (show (toExpr rconf) ) $ mapM memoStep (split rconf)
            splits <- mapM memoStep (split rconf)
            let r@(env', stack', expr') = combine rconf splits
            --return r
            promise v (fvs rconf) r
            return (env', stack', appVars (Var v) fv)

          Just conf' -> memoStep conf'
      else do
        let (var, _) = fromJust ii
        return (env, stack, appVars (Var var) (fvs conf))
  where fvs = freeVars . envExpr

memo :: Conf -> Memo Conf
memo conf@(env, stack, expr) =
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
        (v, fv) <- rec conf

        let rconf'' = reduce conf
        let rconf' = freduce (map (Var . (++) "$m_" . show) [1..10]) rconf''
        let rconf = reduce $ simp rconf'
        splits <- mapM memo (split rconf)
        let r@(env', stack', expr') = combine rconf splits
        promise v (fvs rconf) r
        return (env', stack', appVars (Var v) fv)
      else do
        let (var, _) = fromJust ii
        return (env, stack, appVars (Var var) (fvs conf))
  where fvs = freeVars . envExpr

type Hist = [(Var, [Var], Conf)]

type Prom = [(Var, [Var], Conf)]

type Memo a = State (Int, Hist, Prom) a

envExpr :: Conf -> Expr
envExpr conf@(env, stack, expr) = rebuildEnv env (toExpr conf)
  where rebuildEnv [] expr = expr
        rebuildEnv ((var,valexpr):env) expr =
          Let var valexpr (rebuildEnv env expr)

rec :: Conf -> Memo (Var, [Var])
rec conf = state (\(next, hist, prom) ->
  let var = "$v_" ++ show next in
  let fv = (freeVars . envExpr) conf in
    ((var, fv), (next+1, (var, fv, conf):hist, prom))
  )

promise :: Var -> [Var] -> Conf -> Memo ()
promise var fv conf = state (\(next, hist, prom) -> ((),
    (next, hist, (var, fv, conf):prom))
  )

isin :: Conf -> Match -> Memo (Maybe (Var, [Var]))
isin conf m = state (
    \(next, hist, prom) -> (lookupMatch m hist conf, (next, hist, prom))
  )

getNext :: Memo Int
getNext = state (
    \(next, hist, prom) -> (next, (next, hist, prom))
  )

type Match = Conf -> Conf -> Bool

textualMatch :: Conf -> Conf -> Bool
textualMatch (_,Update _:_,_) (_,_,_)          = False
textualMatch (_,_,_)          (_,Update _:_,_) = False
textualMatch (_,_,l)          (_,_,r)          = l == r

-- | Not alpha-equivalence. Free variables equivalence.
-- | Implementation not nice, but nices.
-- | Support only up to 10 arguments!!!
match :: Conf -> Conf -> Bool
match lhs rhs = toExpr lred == toExpr rred
  where
    lexpr = envExpr lhs
    rexpr = envExpr rhs
    lfv   = freeVars lexpr
    rfv   = freeVars rexpr
    llam  = toLambda lfv lexpr
    rlam  = toLambda rfv rexpr
    lred  = freduce args (newConf emptyEnv llam)
    rred  = freduce args (newConf emptyEnv rlam)
    args  = map (Var . (++) "$a_" . show) [1..10]

lookupMatch :: Match -> Hist -> Conf -> Maybe (Var, [Var])
lookupMatch _ [] _ = Nothing
lookupMatch m ((var, vars, conf'):hist) conf = if conf `m` conf'
  then Just (var, vars)
  else lookupMatch m hist conf

instance {-# OVERLAPPING #-} Show Hist where
  show hist = "" ++
    intercalate "\n" (map ((++) "  " . show) hist)

instance {-# OVERLAPPING #-} Show (Var, [Var], Conf) where
  show (var, args, expr) =
    var ++ "(" ++ unwords args ++ ") ~> " ++ show expr

instance {-# OVERLAPPING #-} Show (Var, Conf) where
  show (var, expr) = var ++ " ~> " ++ show expr

instance {-# OVERLAPPING #-} Show a => Show (a, (Int, Hist, Prom)) where
  show (val, (next, hist, prom)) =
    "Value: " ++ show val ++ "\n" ++
    "Next: " ++ show next ++ "\n" ++
    "Hist: \n" ++ show hist ++ "\n" ++
    "Prom: \n" ++ show prom
