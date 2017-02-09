{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

{-|
  Defines the Supercompiler based on the operational semantics of the
  language using Eval.
-}
module Supercompiler (supercompile, ptree, residuate) where

import Control.Exception (assert)
import Data.Maybe (fromJust)
import Control.Arrow (first, second)

import Expr (Var, Tag, Expr(Var, Con, Let, Case), Pat(Pat), Subst,
             con, app, appVars, isVar, subst, substAlts)
import Eval (Conf, Env, Stack, StackFrame(Arg, Alts), newConf, initConf, step)
import Match ((|~~|), (<|), (|><|))
import Tree (Tree(Node), depth, flatten)

{-|
  Supercompiles an expr.
-}
supercompile :: Expr -> Expr
supercompile = residuate . ptree

{-|
  Given an Expr, builds the corresponding process tree.
-}
ptree :: Expr -> Tree Label (Integer, (Conf, Emit))
ptree = depth 15 . stamp . id . id . path . id . drive . initConf

{-|
  Builds an Expr from the given process tree.
-}
residuate :: Tree Label (Integer, (Conf, Emit)) -> Expr
residuate (Node (key, (_conf, emit)) ts) = case emit of
  EmitNop -> assert (length ts == 1 && fst (head ts) == Step) $
    residuate $ snd (head ts)
  EmitVar var -> assert (null ts) $
    Var var
  EmitCon tag -> app (Con tag []) (map (residuate . snd) ts)
  EmitLet -> assert (length ts >= 2) $
    Let (map (\(LetLabel l, t) -> (l, residuate t)) $ tail ts)
        (residuate . snd $ head ts)
  EmitCase scvar ->
    Case (Var scvar) $ map (\(PatLabel p, t) -> (p, residuate t)) ts
  EmitTie vs -> assert (null ts) $
    appVars (Var fkey) vs
  EmitLam -> assert (length ts == 1 && fst (head ts) == Step) $
    Let [(fkey, residuate $ snd (head ts))] (Var fkey)
  where fkey = "$f_" ++ show key

{-|
  Represents where the expression has been stucked.
-}
data Emit =
  EmitNop |
  EmitVar Var |
  EmitArg | EmitCon Tag | EmitLet | EmitCase Var | EmitTie [Var] | EmitLam

{-|
  Represents how an expression connects to all its splitted childs.
-}
data Label = PatLabel Pat | ConEdge | ArgLabel | Step | LetLabel String
  deriving Eq

instance Show Emit where
  show emit = case emit of
    EmitNop -> "-"
    EmitVar var -> "var:" ++ var
    EmitArg -> "&arg"
    EmitCon tag -> "Con:" ++ tag
    EmitLet -> "let"
    EmitCase var -> "case:" ++ var
    EmitTie vs -> "~~" ++ show vs
    EmitLam -> "lam"

instance Show Label where
  show label = case label of
    PatLabel pat -> '?' : show pat
    ConEdge -> "Con"
    ArgLabel -> "ArgLabel"
    Step -> "Step"
    LetLabel s -> "Let:" ++ s

instance {-# OVERLAPPING #-} Show (Integer, ((Stack, Expr), Emit)) where
  show (k, (x, emit)) = show k ++ "#" ++ show x ++ " %" ++ show emit

{-|
  Given a state, returns where to continue the computation.
  The given conf must be stucked.
-}
split :: Conf -> (Emit, [(Label, Conf)])
split s@(env, stack, expr) = case expr of
  Var var -> case stack of
    [] -> (EmitVar var, [])
    Arg arg:stack' -> (EmitArg, [(ArgLabel, (env, stack', arg))])
    Alts alts:stack' -> (EmitCase var,
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
    [] -> (EmitCon tag, map ((,) ConEdge . newConf env) args)
    _ -> error $ "Spliting with Con and stack: " ++ show stack

-- freduce :: Conf -> Conf
-- freduce = freduce' 1
--   where
--    v = Var . (++) "$m_" . show
--    freduce' next conf = let (env, stack, expr) = reduce conf in
--     case expr of
--       Lam var lamexpr ->
--         case stack of
--           [] -> freduce' (next + 1) (env, [Arg (v next)], Lam var lamexpr)
--           _ -> error $ "Stack not empty in freduce" ++ show stack
--       _ -> (env, stack, expr)

drive :: Conf -> Tree Label (Conf, Emit)
drive conf = case step conf of
  Nothing -> let s = split conf in
    Node (conf, fst s) (map (second drive) $ snd s)
  Just conf' -> Node (conf, EmitNop) [(Step, drive conf')]

path :: Tree e a -> Tree e ([Int], a)
path = path' [1]
  where
    path' :: [Int] -> Tree e a -> Tree e ([Int], a)
    path' key (Node x cs) = let n = length cs in
      Node (key, x) $ zipWith (\k (e, v) -> (e, path' (k:key) v)) [1..n] cs

stamp :: Eq k => Tree e (k, a) -> Tree e (Integer, a)
stamp t = fmap (first $ flookup $ zip (map fst $ flatten t) [0..]) t
  where
    flookup :: Eq k => [(k, a)] -> k -> a
    flookup d k = fromJust (lookup k d)

generalize :: Tree Label (Conf, Emit) -> Tree Label (Conf, Emit)
generalize = generalize' []
  where
    generalize' :: [Conf] -> Tree Label (Conf, Emit) -> Tree Label (Conf, Emit)
    generalize' xs (Node (conf@(env,_,_), emit) ts) = case findGen conf xs of
      Nothing -> Node (conf, emit) (fmap (second $ generalize' (conf:xs)) ts)
      Just (eg, _s, s') -> Node (conf, EmitLet) $
        map (first LetLabel . second (generalize.drive.newConf env )) $ ("",eg):s'
    findGen :: Conf -> [Conf] -> Maybe (Expr, [Subst], [Subst])
    findGen _ [] = Nothing
    findGen conf (c:cs) = let x@(e,_,_) = g c conf in
      if isEmb conf c -- && not (isVar e)
      then Just x
      else findGen conf cs
    isEmb :: Conf -> Conf -> Bool
    isEmb (_, [], e) (_, [], e') = e' <| e
    isEmb _ _ = False
    g :: Conf -> Conf -> (Expr, [Subst], [Subst])
    g (_, _, e) (_, _, e') = e |><| e'

tie :: Tree e (k, (Conf, Emit)) -> Tree e (k, (Conf, Emit))
tie = tie' []
  where
    tie' :: [(k, Conf)] -> Tree e (k, (Conf, Emit)) -> Tree e (k, (Conf, Emit))
    tie' xs (Node (key, (conf, emit)) cs) = case findUni conf xs of
      Nothing -> Node (key, (conf, emit)) (fmap (second $ tie' ((key,conf):xs)) cs)
      Just (k, ss) -> Node (k, (conf, EmitTie (map fst ss) )) []
    findUni :: Conf -> [(k, Conf)] -> Maybe (k, [Subst])
    findUni _ [] = Nothing
    findUni c@(env, s, e) ((key,(env', s', e')):cs) = case e |~~| e' of
      Nothing -> findUni c cs
      Just ss -> if null s && null s' &&
        all (\(v,ee)->isVar ee && n v env && n (let Var v'=ee in v') env') ss
        then Just (key, ss)
        else findUni c cs
    n :: Var -> Env -> Bool
    n v ls = v `notElem` (fst . unzip) ls

back :: Eq k => Tree e (k, (Conf, Emit)) -> Tree e (k, (Conf, Emit))
back t0 = fmap (updateNode $ backKeys t0) t0
  where
    backKeys :: Tree e (k, (Conf, Emit)) -> [k]
    backKeys t = map fst $ filter (isTie . snd . snd) (flatten t)
    isTie :: Emit -> Bool
    isTie (EmitTie _) = True
    isTie _ = False
    updateNode :: Eq k => [k] -> (k, (Conf, Emit)) -> (k, (Conf, Emit))
    updateNode ks node@(key, (conf, emit)) = if key `elem` ks && not (isTie emit)
      then (key, (conf, EmitLam))
      else node
