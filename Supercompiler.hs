{-# LANGUAGE FlexibleInstances #-}

module Supercompiler where

import Data.List (intercalate)
import Data.Maybe (isNothing, fromJust)
import Control.Exception (assert)
import Control.Monad.State (State, state, runState)

import Expr (Expr(..), Var, Pat(Pat), app, appVars, isVar, isEmptyCon, freeVars)
import Eval (Conf, Env, StackFrame(..),
  newConf, emptyEnv, toExpr, nf, reduce, put)
import Splitter (
  Node(VarNode, ArgNode, ConNode, CaseNode),
  Label(PatLabel, ConLabel),
  split)
import Simplifier (freduce, simp)
import Match (Match, match, toLambda, envExpr)

supercompile :: Expr -> Expr
supercompile = gp . runMemo
  where
    gp (expr0, (_, prom)) = fromMemo prom expr0
    fromMemo [] expr0 = expr0
    fromMemo ((var, expr):prom) expr0 =
      Let var expr (fromMemo prom expr0)

runMemo :: Expr -> (Expr, (Hist, Env))
runMemo expr = runState s (([], []), [])
  where s = memo "$v_start" (ConLabel "_") (newConf emptyEnv expr)

-- Runs the supercompiler
-- Conf has to be WHNF?
memo :: Var -> Label -> Conf -> Memo Expr
memo parentVar label conf@(env, stack, expr) =
  do
  next <- getNext
  if next > 10 then return expr else --error "Reached 100 iterations, implement termination" else
    do
    ii <- isin conf match
    if isNothing ii
      then do
        --let rconf@(_, _, vv) = reduce $ simp $ reduce $ freduce $ reduce conf
        let rconf@(_, _, vv) = reduce $ freduce $ reduce conf
        let (node, sps) = split rconf
        let fv = fvs rconf
        v <- rec parentVar label fv node rconf

        splits <- mapM (uncurry $ memo v) sps
        let e = toLambda fv $ case node of
                  VarNode -> vv
                  ArgNode -> app vv splits
                  ConNode -> let Con tag args = vv in app (Con tag []) splits
                  CaseNode -> let alts = zip (fst (unzip sps)) splits in
                    Case vv (map (\(PatLabel p,e)-> (p, e)) alts)

        promise v e
        return $ appVars (Var v) (fvs conf)
      else do
        let (var, _) = fromJust ii
        recArrow parentVar label var
        return $ appVars (Var var) (fvs conf)
  where fvs = freeVars . envExpr

type Hist = ([(Var, Label, Var)], [(Var, [Var], Node, Conf)])

type Memo a = State (Hist, Env) a

rec :: Var -> Label -> [Var] -> Node -> Conf -> Memo Var
rec parentVar label fv node conf = state $ \((es, vs), prom) ->
  let var = "$v_" ++ show (length vs) in
    (var, (((parentVar, label, var):es, (var, fv, node, conf):vs), prom))

recArrow :: Var -> Label -> Var -> Memo ()
recArrow parentVar label var = state $ \((es, vs), prom) ->
    ((), (((parentVar, label, var):es, vs), prom))

promise :: Var -> Expr -> Memo ()
promise var expr = state $ \(hist, prom) ->
  ((), (hist, (var, expr):prom))

isin :: Conf -> Match -> Memo (Maybe (Var, [Var]))
isin conf m = state $ \(hist@(es, vs), prom) ->
  (lookupMatch m vs conf, (hist, prom))

getNext :: Memo Int
getNext = state $ \(hist@(es, vs), prom) -> (length vs, (hist, prom))

lookupMatch :: Match -> [(Var, [Var], Node, Conf)] -> Conf -> Maybe (Var, [Var])
lookupMatch _ [] _ = Nothing
lookupMatch m ((var, vars, node, conf'):hist) conf =
  --trace (show conf ++ " ?==? " ++ var ++ show conf') $
  if conf `m` conf'
    then Just (var, vars)
    else lookupMatch m hist conf

instance {-# OVERLAPPING #-} Show Hist where
  show (es, vs) = "" ++
    intercalate "\n" (map ((++) "  " . show) es) ++ "\n" ++
    intercalate "\n" (map ((++) "  " . show) vs)

-- instance {-# OVERLAPPING #-} Show Prom where
--   show prom = "" ++
--     intercalate "\n" (map ((++) "  " . show) prom)

instance {-# OVERLAPPING #-} Show (Var, Label, Var) where
  show (parentVar, label, var) = parentVar ++ "/"++show label++"/" ++ var

instance {-# OVERLAPPING #-} Show (Var, [Var], Expr) where
  show (var, args, expr) =
    var ++ "(" ++ unwords args ++ ") ~> " ++ show expr

instance {-# OVERLAPPING #-} Show (Var, [Var], Node, Conf) where
  show (var, args, node, expr) =
    var ++ "(" ++ unwords args ++ ") ~> " ++ show node ++ "@" ++ show expr

instance {-# OVERLAPPING #-} Show a => Show (a, (Hist, Env)) where
  show (val, (hist, prom)) =
    "Value: " ++ show val ++ "\n" ++
    "Hist: \n" ++ show hist ++ "\n" ++
    "Prom: \n" ++ show prom
