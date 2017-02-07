{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

{-|
  Defines the Supercompiler based on the operational semantics of the
  language using Eval.
-}
module Supercompiler (
  Node(VarNode, ArgNode, ConNode, CaseNode), Label, process
) where

import Data.List (find, intercalate)
import Data.Maybe (isJust, fromJust)
import Control.Arrow (second)

import Expr (Var, Expr(Var, Con), Pat(Pat), Subst,
             con, appVars, isVar, isLet, isCase, subst, substAlts)
import Eval (Conf, Env, StackFrame(Arg, Alts), newConf, initConf, step)
import Match ((|~~|), (<|), (|><|))
import Tree (Tree(Node), depth)

import Debug.Trace

{-|
  Represents where the expression has been stucked.
-}
data Node = VarNode | ArgNode | ConNode | CaseNode deriving Show

{-|
  Represents how an expression connects to all its splitted childs.
-}
data Label = PatLabel Pat | ConLabel String | ArgLabel | Step | Let String
  deriving Eq

instance Show Label where
  show label = case label of
    PatLabel pat -> '?' : show pat
    ConLabel s -> s
    ArgLabel -> "ArgLabel"
    Step -> "Step"
    Let s -> "Let:" ++ s

process :: Expr -> Tree Label (Key, Conf)
process = depth 24 . stamp . generalize . drive . initConf

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

drive :: Conf -> Tree Label Conf
drive conf = case step conf of
  Nothing -> Node conf (map (second drive) $ (snd . split) conf)
  Just conf' -> Node conf [(Step, drive conf')]

type Key = [Int]

initKey :: Key
initKey = [1]

instance {-# OVERLAPPING #-} Show a => Show (Key, a) where
  show (key, x) = show x -- intercalate "" (map show key) ++ "#" ++ show x
    -- where
    --   getKey :: Key -> Int
    --   getKey [] = 0
    --   getKey ((k, n):xs) = k*n + getKey xs

stamp :: Tree e a -> Tree e (Key, a)
stamp = stamp' initKey
  where
    stamp' :: Key -> Tree e a -> Tree e (Key, a)
    stamp' key (Node x cs) = let n = length cs in
      Node (key, x) $ zipWith (\k (e, v) -> (e, stamp' (k:key) v)) [1..n] cs

generalize :: Tree Label Conf -> Tree Label Conf
generalize = generalize' []
  where
    generalize' :: [Conf] -> Tree Label Conf -> Tree Label Conf
    generalize' xs (Node conf ts) = let c = findGen conf xs in if isJust c
      then
            let x@(eg, s, s') = fromJust c
                (env, _, _) = conf
                cg = newConf env eg in
            Node (conf) [(Let $ show x, (generalize . drive) cg)]
          else Node conf (fmap (second $ generalize' (conf:xs)) ts)
    findGen :: Conf -> [Conf] -> Maybe (Expr, [Subst], [Subst])
    findGen conf [] = Nothing
    findGen conf (c:cs) = let x@(e,_,_) = g c conf in
      if isEmb conf c -- && not (isVar e)
      then Just x
      else findGen conf cs
    isEmb :: Conf -> Conf -> Bool
    isEmb (_, [], e) (_, [], e') = e' <| e
    isEmb _ _ = False
    g :: Conf -> Conf -> (Expr, [Subst], [Subst])
    g (_, _, e) (_, _, e') = let x@(eg, s, s') = e |><| e' in x

tie :: Tree e (Key, Conf) -> Tree e (Key, Conf)
tie = tie' []
  where
    tie' :: [(Key, Conf)] -> Tree e (Key, Conf) -> Tree e (Key, Conf)
    tie' xs (Node (id, conf) cs) = let c = find (uni conf) xs in if isJust c
          then let cc=fromJust c in Node (fst cc, conf)[]
          else Node (id, conf) (fmap (second $ tie' ((id,conf):xs)) cs)
    uni :: Conf -> (Key, Conf) -> Bool
    uni (env, s, e) (id,(env', s', e')) = case e |~~| e' of
      Nothing -> False
      Just xs -> null s && null s' && all (\(v,e)->isVar e && n v env && n (let Var v'=e in v') env') xs
    n :: Var -> Env -> Bool
    n v ls = v `notElem` (fst . unzip) ls

-- residuate (Node (id, (_,s,e)) cs) = case e of
--   Var var ->
