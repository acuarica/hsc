{-# LANGUAGE FlexibleInstances #-}

module Supercompiler where

import Data.List (intercalate, union)
import Data.Maybe (isNothing, fromJust)
import Control.Exception (assert)
import Control.Monad.State (State, state, runState)

import Expr (
  Expr(..), Var, Pat(Pat),
  app, appVars, isVar, isEmptyCon, freeVars)
import Eval (
  Conf, Env, StackFrame(..),
  newConf, emptyEnv, toExpr, nf, reduce, put)
import Splitter (
  Node(VarNode, ArgNode, ConNode, CaseNode),
  Label(PatLabel, ConLabel),
  split)
import Simplifier (freduce, simp)
import Match -- (match, toLambda, envExpr)

supercompile :: Expr -> Expr
supercompile = fst . supercompileWithMemo

supercompileWithMemo :: Expr -> (Expr, ((Var, Expr), (Hist, Env)))
supercompileWithMemo expr =
  let rm = runMemo expr in
  let gp ((_, expr0), (_, prom)) = fromMemo prom expr0 in
  (gp rm, rm)

fromMemo [] expr0 = expr0
fromMemo ((var, expr):prom) expr0 = Let var expr (fromMemo prom expr0)

runMemo :: Expr -> ((Var, Expr), (Hist, Env))
runMemo expr = runState s (([], []), [])
  where s = memo (newConf emptyEnv expr)

-- Runs the supercompiler
-- Conf has to be WHNF?
memo :: Conf -> Memo (Var, Expr)
memo conf@(env, stack, expr) =
  do
  next <- getNext
  if next > 100 then return ("??", expr) else --error "100 iterations, implement termination" else
    do
    ii <- isin conf
    if isNothing ii
      then do
        let rconf@(_, _, vv) = reduce $ freduce $ reduce conf
        let (node, sps) = split rconf
        let fv = fvs rconf
        --let fv = fvs conf
        v <- recVertex fv node rconf sps
        --v <- rec parentVar label fv node rconf (snd $ unzip sps)

        splits' <- mapM (memo . snd) sps
        let (childVars, splits) = unzip splits'
        let ccs = zipWith (\(l,c) v -> (l, v)) sps childVars

        mapM_ (\(l,cv)->recEdge (v, l, cv, fvs conf, conf)) ccs

        let fvl = fvs (reduce conf) `union` fvs rconf
        let e = toLambda fvl $ case node of
              VarNode -> vv
              ArgNode -> app vv splits
              ConNode -> let Con tag args = vv in app (Con tag []) splits
              CaseNode -> let alts = zip (fst (unzip sps)) splits in
                Case vv (map (\(PatLabel p,e)-> (p, e)) alts)

        promise v e
        return (v, appVars (Var v) (fvs $ reduce conf))
        --return (v, appVars (Var v) fv)
      else do
        let (var, fv) = fromJust ii
        --recEdge (parentVar, label, var, fvs conf, conf)
        return (var, appVars (Var var) (fvs $ reduce conf))
        --return (var, appVars (Var var) (fvs conf))
  where fvs = freeVars . envExpr

type HistEdge = (Var, Label, Var, [Var], Conf)

type HistNode = (Var, [Var], Node, Conf, [(Label, Conf)])

type Hist = ([HistEdge], [HistNode])

type Memo a = State (Hist, Env) a

-- rec :: Var -> Label -> [Var] -> Node -> Conf -> [Conf] -> Memo Var
-- rec parentVar label fv node conf sps = state $ \((es, vs), prom) ->
--   let var = "$v_" ++ show (length vs) in
--   let edge = (parentVar, label, var, fv, conf) in
--   let vertex = (var, fv, node, conf, sps) in
--     (var, ((edge:es, vertex:vs), prom))

recVertex :: [Var] -> Node -> Conf -> [(Label, Conf)] -> Memo Var
recVertex fv node conf sps = state $ \((es, vs), prom) ->
  let var = "$v_" ++ show (length vs) in
  let vertex = (var, fv, node, conf, sps) in
    (var, ((es, vertex:vs), prom))

recEdge :: HistEdge -> Memo ()
recEdge edge = state $ \((es, vs), prom) ->
    ((), ((edge:es, vs), prom))

promise :: Var -> Expr -> Memo ()
promise var expr = state $ \(hist, prom) ->
  ((), (hist, (var, expr):prom))

isin :: Conf -> Memo (Maybe (Var, [Var]))
isin conf = state $ \(hist@(es, vs), prom) ->
  (lookupMatch vs conf, (hist, prom))

getNext :: Memo Int
getNext = state $ \(hist@(es, vs), prom) -> (length vs, (hist, prom))

lookupMatch :: [HistNode] -> Conf -> Maybe (Var, [Var])
lookupMatch [] _ = Nothing
lookupMatch ((var, vars, node, conf', sps):hist) conf =
  if match conf conf'
    then if False --fvs (reduce conf) /= fvs (reduce conf')
      then error $ show (fvs $ reduce conf) ++ show (fvs $ reduce conf') ++ "\n" ++ show conf ++ "\n"++ show conf' ++ "\n" ++
        show (match' conf) ++ "\n" ++ show (match' conf') ++ "\n" ++
        show (freeVars $ match' conf) ++ "\n" ++ show (freeVars $ match' conf')
      else Just (var, vars)
    else lookupMatch hist conf
  where fvs = freeVars . envExpr

instance {-# OVERLAPPING #-} Show Hist where
  show (es, vs) = "" ++
    intercalate "\n" (map ((++) "  " . show) es) ++ "\n" ++
    intercalate "\n" (map ((++) "  " . show) vs)

-- instance {-# OVERLAPPING #-} Show Prom where
--   show prom = "" ++
--     intercalate "\n" (map ((++) "  " . show) prom)

instance {-# OVERLAPPING #-} Show HistEdge where
  show (parentVar, label, var, fv, conf) =
    parentVar ++ "/"++show label++"/" ++ var ++ unwords fv ++ show conf

instance {-# OVERLAPPING #-} Show HistNode where
  show (var, args, node, expr, sps) =
    var ++ "(" ++ unwords args ++ ") ~> " ++ show node ++ "@" ++
    show expr ++ "\n    " ++ intercalate "\n    " (map show sps)

-- instance {-# OVERLAPPING #-} Show (Var, [Var], Expr) where
--   show (var, args, expr) =
--     var ++ "(" ++ unwords args ++ ") ~> " ++ show expr

instance {-# OVERLAPPING #-} Show ((Var, Expr), (Hist, Env)) where
  show ((var0, expr0), (hist, prom)) =
    "Var0/Expr0: " ++ var0 ++ "/" ++ show expr0 ++"\n" ++
    "Hist: \n" ++ show hist ++ "\n" ++
    "Prom: \n" ++ show prom
