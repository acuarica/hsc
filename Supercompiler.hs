{-# LANGUAGE FlexibleInstances #-}

module Supercompiler where

import Data.List (intercalate)
import Data.Maybe (isNothing, fromJust)
import Control.Exception (assert)
import Control.Monad.State (State, state, runState)

import Expr (Expr(..), Var, Pat(Pat), app, appVars, isVar, isEmptyCon, freeVars)
import Eval (Conf, Env, StackFrame(..),
  newConf, emptyEnv, toExpr, nf, reduce, put)
import Splitter (Node(VarNode, ConNode, CaseNode), Label(PatLabel, ConLabel),
  split, combine)
import Simplifier (doSimp, freduce, simp)
import Match (Match, match, toLambda, envExpr)

import Debug.Trace ()

supercompile :: Expr -> Expr
supercompile = gp . runMemo

gp :: (Expr, (Hist, Prom)) -> Expr
gp (expr0, (_, prom)) = fromMemo prom expr0

runMemo :: Expr -> (Expr, (Hist, Prom))
runMemo expr = let s = memo "$v_start" (ConLabel "_") (newConf emptyEnv expr)
  in runState s (([], []), [])

fromMemo :: [(Var, [Var], Expr)] -> Expr -> Expr
fromMemo [] expr0 = expr0
fromMemo ((var, fv, expr):prom) expr0 =
    Let var expr (fromMemo prom expr0)

add :: Var -> Expr -> Conf -> Conf
add var valexpr (env, stack, expr) = (put var valexpr env, stack, expr)

-- Runs the supercompiler
-- Conf has to be WHNF?
memo :: Var -> Label -> Conf -> Memo Expr
memo parentVar label conf@(env, stack, expr) =
  do
  next <- getNext
  if next > 100 then error "Reached 100 iterations, implement termination" else
    do
    ii <- isin conf match
    if isNothing ii
      then do
        --let rconf = reduce $ simp $ reduce conf
        --let rconf = doSimp conf
        let rconf@(_, _, vv) = reduce conf
        let (node, sps) = split rconf
        (v, fv) <- rec parentVar label node rconf

        --recProc conf sps
        splits <- mapM (uncurry $ memo v) sps
        --let r@(env', stack', expr') = combine rconf splits
        let e = toLambda fv $ case node of
                  VarNode -> vv
                  ConNode -> let Con tag args = vv in app (Con tag []) splits
                  CaseNode -> let alts = zip (fst (unzip sps)) splits in
                    Case vv (map (\(PatLabel p,e)-> (p, e)) alts)

        promise v (fvs rconf) e
        --return (env', stack', appVars (Var v) fv)
        -- return $ (
        --   case node of
        --     CaseNode -> Case vv (map (\e-> (Pat "?" [], e)) splits)
        --     _ -> Var "??"
        --   )
        return $ appVars (Var v) fv
      else do
        let (var, _) = fromJust ii
        recArrow parentVar label var
        --return (env, stack, appVars (Var var) (fvs conf))
        return $ appVars (Var var) (fvs conf)
  where fvs = freeVars . envExpr

--type Hist = [(Var, Label, Var, [Var], Conf)]
type Hist = ([(Var, Label, Var)], [(Var, [Var], Node, Conf)])

type Prom = [(Var, [Var], Expr)]

type Memo a = State (Hist, Prom) a

rec :: Var -> Label -> Node -> Conf -> Memo (Var, [Var])
rec parentVar label node conf = state $ \((es, vs), prom) ->
  let var = "$v_" ++ show (length vs) in
  let fv = (freeVars . envExpr) conf in
    ((var, fv), (((parentVar, label, var):es, (var, fv, node, conf):vs), prom))

recArrow :: Var -> Label -> Var -> Memo ()
recArrow parentVar label var = state $ \((es, vs), prom) ->
    ((), (((parentVar, label, var):es, vs), prom))

promise :: Var -> [Var] -> Expr -> Memo ()
promise var fv expr = state $ \(hist, prom) ->
  ((), (hist, (var, fv, expr):prom))

isin :: Conf -> Match -> Memo (Maybe (Var, [Var]))
isin conf m = state $ \(hist@(es, vs), prom) ->
  (lookupMatch m vs conf, (hist, prom))

getNext :: Memo Int
getNext = state $ \(hist, prom) -> (length hist, (hist, prom))

lookupMatch :: Match -> [(Var, [Var], Node, Conf)] -> Conf -> Maybe (Var, [Var])
lookupMatch _ [] _ = Nothing
lookupMatch m ((var, vars, node, conf'):hist) conf = if conf `m` conf'
  then Just (var, vars)
  else lookupMatch m hist conf

instance {-# OVERLAPPING #-} Show Hist where
  show (es, vs) = "" ++
    intercalate "\n" (map ((++) "  " . show) es) ++ "\n" ++
    intercalate "\n" (map ((++) "  " . show) vs)

instance {-# OVERLAPPING #-} Show Prom where
  show prom = "" ++
    intercalate "\n" (map ((++) "  " . show) prom)

instance {-# OVERLAPPING #-} Show (Var, Label, Var) where
  show (parentVar, label, var) = parentVar ++ "/"++show label++"/" ++ var

instance {-# OVERLAPPING #-} Show (Var, [Var], Expr) where
  show (var, args, expr) =
    var ++ "(" ++ unwords args ++ ") ~> " ++ show expr

instance {-# OVERLAPPING #-} Show (Var, [Var], Node, Conf) where
  show (var, args, node, expr) =
    var ++ "(" ++ unwords args ++ ") ~> " ++ show node ++ "@" ++ show expr

instance {-# OVERLAPPING #-} Show (Var, Conf) where
  show (var, expr) = var ++ " ~> " ++ show expr

instance {-# OVERLAPPING #-} Show a => Show (a, (Hist, Prom)) where
  show (val, (hist, prom)) =
    "Value: " ++ show val ++ "\n" ++
    --"Next: " ++ show next ++ "\n" ++
    "Hist: \n" ++ show hist ++ "\n" ++
    "Prom: \n" ++ show prom
