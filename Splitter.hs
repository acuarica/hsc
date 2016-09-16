
module Splitter (
    Node(VarNode, ArgNode, ConNode, CaseNode),
    Label(PatLabel, ConLabel),
    split
  ) where

import Expr --(Expr(Var, Con, Lam, App), Pat(Pat), con, appVars, subst)
import Eval --(Conf, StackFrame(Arg, Alts), newConf, toExpr, reduce)
import Match (toLambda)
import Data.Maybe

-- |
-- |
data Node = VarNode | ArgNode | ConNode | CaseNode deriving Show

-- |
-- |
data Label = PatLabel Pat | ConLabel String | ArgLabel

instance Show Label where
  show (PatLabel pat) = show pat
  show (ConLabel s) = s
  show ArgLabel = "ArgLabel"

-- | Given a state, returns where to continue the computation.
-- | The given conf must be stucked.
split :: Conf -> (Node, [(Label, Conf)])
split s@(env, stack, expr) = case expr of
  Var var -> case stack of
    [] -> (VarNode, [])
    Arg arg:stack' -> (ArgNode, [(ArgLabel, (env, stack', arg))])
    Alts alts:stack' -> (CaseNode,
      map (\(Pat tag vars, alt) ->
        --let cVar v i = "$c_" ++ show i ++ "_" ++ v in
        let cVar v i = "$" ++ v in
        let cVars = zipWith cVar vars [1..length vars] in
        let repl = zip vars (map Var cVars) in
        let cc = appVars (con tag) cVars in
        let altExpr = subst (var, cc) $ substAlts repl alt in
        let altFrame frame = (case frame of
              Alts alts' -> Alts (map (\(p,e)->(p,subst (var, cc) e)) alts') ) in
        let altStack = map altFrame stack' in
        let altConf = (env, altStack, altExpr) in
        (PatLabel (Pat tag cVars), altConf) ) alts)
    _ -> error $ "Error: split var: " ++ show s
  Con tag args -> case stack of
    [] -> (ConNode,
      map (\(i, e)->
        (ConLabel $ tag ++ "_" ++ show i, newConf env e)) (zip [1..length args] args))
    _ -> error $ "Spliting with Con and stack: " ++ show stack
