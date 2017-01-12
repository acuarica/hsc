{-# LANGUAGE FlexibleInstances #-}

{-|
  Defines the Supercompiler based on the operational semantics of the
  language using Eval.
-}
module Supercompiler (
  Node(VarNode, ArgNode, ConNode, CaseNode),
  Hist, HistNode, HistEdge, supercompile, supercompileMemo
) where

import Data.List (intercalate, union, nub)
import Data.Maybe (isNothing, fromJust)
-- import Control.Exception (assert)
import Control.Monad.State (State, state, runState)
import Control.Arrow (second)

import Expr (
  Expr(Var, Con, Let, Case, Lam, App), Var, Pat(Pat),
  con, app, appVars, let1, isVar, freeVars, subst, substAlts)
import Eval (
  Conf, Env, StackFrame(Arg, Alts),
  newConf, emptyEnv, toExpr, reduce, whnf)
import Match (match, toLambda, envExpr, freduce, (<|), (|><|))

{-|
  Supercompiles an Expr.
-}
supercompile :: Expr -> Expr
supercompile = fst . supercompileMemo

{-|
  Supercompiles an Expr, and also returns the memo.
-}
supercompileMemo :: Expr -> (Expr, ((Var, Expr), (Hist, Env)))
supercompileMemo expr =
  let rm = runMemo expr in
  let gp ((_, expr0), (_, prom)) = fromMemo prom expr0 in
  (gp rm, rm)

{-|
  Rebuilds an expression from the promises.
-}
fromMemo :: Env -> Expr -> Expr
fromMemo [] expr0 = expr0
fromMemo ((var, expr):prom) expr0 = let1 var expr (fromMemo prom expr0)

{-|
  Runs the state machine for memo/hist.
-}
runMemo :: Expr -> ((Var, Expr), (Hist, Env))
runMemo expr = runState s (([], []), [])
  where s = memo [] (newConf emptyEnv expr)

isCon :: Conf -> Bool
isCon (_, _, Con _ _) = True
isCon _ = False

isVar' :: Conf -> Bool
isVar' (_, _, Var _) = True
isVar' _ = False

{-|
  Runs the supercompiler
  Conf has to be WHNF?
-}
memo :: [Var] -> Conf -> Memo (Var, Expr)
memo path conf@(_env, _stack, expr) =
  do
  next <- getNext
  if next > 20
    then return ("??", expr)
    else
    do
    ii <- isin conf
    if isNothing ii
      then do
        ee <- embin path conf
        let genIsVarOrNoEmb = if isNothing ee
              then True
              else let (var, fv, gconf) = fromJust ee
                       (gexpr, s, t) = doExpr gconf |><| doExpr conf
                    in isVar gexpr
        -- FIXME: Merge this if w/ HE and generalization. Use only generalization.
        if isNothing ee || isVar expr || isCon (reduce conf)
          || genIsVarOrNoEmb || next < 10
          then do
            let rconf@(_, _, vv) = reduce $ freduce $ reduce conf
            let (node, sps) = split rconf
            let fv = fvs rconf
            --let fv = fvs conf
            v <- recVertex fv node rconf sps

            splits' <- mapM (memo (v:path) . snd) sps
            let (childVars, splits) = unzip splits'
            let ccs = zipWith (\(l, _c) u -> (l, u)) sps childVars

            mapM_ (\(l,cv)->recEdge (v, l, cv, fvs conf, conf)) ccs

            let fvl = fvs (reduce conf) `union` fvs rconf
            let e = toLambda fvl $ case node of
                  VarNode -> vv
                  ArgNode -> app vv splits
                  ConNode -> let Con tag _args = vv in app (Con tag []) splits
                  CaseNode -> let alts = zip (fst (unzip sps)) splits in
                    Case vv (map (\(PatLabel p,pe)-> (p, pe)) alts)

            promise v e
            return (v, appVars (Var v) (fvs $ reduce conf))
            --return (v, appVars (Var v) fv)
          else do
            -- TODO: Apply generalization.
            let (var, fv, gconf) =  -- traceShow (reduce conf) $
                  fromJust ee
            let gen@(gexpr, s, t) = doExpr gconf |><| doExpr conf
            return $ -- traceShow gen
            -- let t' = (nub t)
             ("EMB:" ++ var ++ "/" ++ unwords fv,
              -- Let (nub t) $
              App (Lam "$_" (Var "$_")) $
              app (Var var)
                     --(fvs $ reduce conf)
                     -- (  nub  ((fst . unzip) t)  )
                     $ (snd . unzip) (nub t)
                   )
      else do
        let (var, _fv) = fromJust ii
        --recEdge (parentVar, label, var, fvs conf, conf)
        return (var, appVars (Var var) (fvs $ reduce conf))
        --return (var, appVars (Var var) (fvs conf))
  where fvs = freeVars . envExpr

{-|
  Represents where the expression has been stucked.
-}
data Node = VarNode | ArgNode | ConNode | CaseNode deriving Show

{-|
  Represents how an expression connects to all its splitted childs.
-}
data Label = PatLabel Pat | ConLabel String | ArgLabel

instance Show Label where
  show (PatLabel pat) = show pat
  show (ConLabel s) = s
  show ArgLabel = "ArgLabel"

-- reduce' :: Conf -> a
-- reduce' conf =
--   let rconf@(_, _, vv) = reduce $ freduce $ reduce conf in
--   let (node, sps) = split rconf in
--   let splits' = map (memo . snd) sps in error ""

{-|
  Given a state, returns where to continue the computation.
  The given conf must be stucked.
-}
split :: Conf -> (Node, [(Label, Conf)])
split s@(env, stack, expr) = case expr of
  Var var -> case stack of
    [] -> (VarNode, [])
    Arg arg:stack' -> (ArgNode, [(ArgLabel, (env, stack', arg))])
    Alts alts:stack' -> (CaseNode,
      map (\(Pat tag vars, alt) ->
        let cVar v = "$" ++ var ++ "_" ++ v in
        let cVars = map cVar vars in
        let repl = zip vars (map Var cVars) in
        let cc = appVars (con tag) cVars in
        let altExpr = subst (var, cc) $ substAlts repl alt in
        let altFrame frame = (case frame of
              Alts alts' ->
                Alts (map (second (subst (var, cc))) alts') ) in
        let altStack = map altFrame stack' in
        let altConf = (env, altStack, altExpr) in
        (PatLabel (Pat tag cVars), altConf) ) alts)
    _ -> error $ "Error: split var: " ++ show s
  Con tag args -> case stack of
    [] -> (ConNode,
      map (\(i, e)->
        (ConLabel $ tag ++ "_" ++ show i, newConf env e))
          (zip [1..length args] args))
    _ -> error $ "Spliting with Con and stack: " ++ show stack

{-|
  Represents a node in the history graph.
-}
type HistNode = (Var, [Var], Node, Conf, [(Label, Conf)])

{-|
  Represents an edge in the history graph.
-}
type HistEdge = (Var, Label, Var, [Var], Conf)

{-|
  Represents the history graph of memoized expression being supercompiled.
-}
type Hist = ([HistEdge], [HistNode])

{-|
-}
type Memo a = State (Hist, Env) a

{-|
-}
recVertex :: [Var] -> Node -> Conf -> [(Label, Conf)] -> Memo Var
recVertex fv node conf sps = state $ \((es, vs), prom) ->
  let var = "$v_" ++ show (length vs) in
  let vertex = (var, fv, node, conf, sps) in
    (var, ((es, vertex:vs), prom))

{-|
-}
recEdge :: HistEdge -> Memo ()
recEdge edge = state $ \((es, vs), prom) ->
    ((), ((edge:es, vs), prom))

{-|
-}
promise :: Var -> Expr -> Memo ()
promise var expr = state $ \(hist, prom) ->
  ((), (hist, (var, expr):prom))

{-|
-}
isin :: Conf -> Memo (Maybe (Var, [Var]))
isin conf = state $ \(hist@(_es, vs), prom) ->
  (lookupMatch vs conf, (hist, prom))

{-|
-}
embin :: [Var] -> Conf -> Memo (Maybe (Var, [Var], Conf))
embin path conf =
  -- trace ("**" ++ show path) $
  -- trace ("  -" ++ show (doExpr conf)) $
  state $ \(hist@(_es, vs), prom) ->
  (lookupEmb (filter (\(v,_,_,_,_)->v `elem` path) vs) conf, (hist, prom))

-- where
doExpr = toExpr . reduce

lookupEmb :: [HistNode] -> Conf -> Maybe (Var, [Var], Conf)
lookupEmb [] _c = Nothing
lookupEmb vs@((var, vars, _node, conf', _sps):hist) conf =
  -- trace ("  >" ++ var ++ ":" ++ show (doExpr conf') ++ "") $
  if doExpr conf' <| doExpr conf || doExpr conf <| doExpr conf' -- || True
    then if False -- True -- False --fvs (reduce conf) /= fvs (reduce conf')
      then error $
        show (toExpr conf) ++ "\n"++
        show (toExpr conf') ++ "\n" ++
        show vs
      else Just (var, vars, conf')
    else lookupEmb hist conf
  where
    _fvs = freeVars . envExpr
    doExpr = toExpr . reduce

{-|
-}
getNext :: Memo Int
getNext = state $ \(hist@(_es, vs), prom) -> (length vs, (hist, prom))

{-|
-}
lookupMatch :: [HistNode] -> Conf -> Maybe (Var, [Var])
lookupMatch [] _ = Nothing
lookupMatch ((var, vars, _node, conf', _sps):hist) conf =
  if match conf conf'
    then if False --fvs (reduce conf) /= fvs (reduce conf')
      then error $ show (fvs $ reduce conf) ++
        show (fvs $ reduce conf') ++ "\n" ++
        show conf ++ "\n"++ show conf' ++ "\n" -- ++
        -- show (match' conf) ++ "\n" ++ show (match' conf') ++ "\n" ++
        -- show (freeVars $ match' conf) ++ "\n" ++
        -- show (freeVars $ match' conf')
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
