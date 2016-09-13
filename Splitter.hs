
module Splitter (
    Node(VarNode, ArgNode, ConNode, CaseNode),
    Label(PatLabel, ConLabel), 
    split
  ) where

import Expr (Expr(Var, Con, Lam), Pat)
import Eval (Conf, StackFrame(Arg, Alts), newConf, toExpr)

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
      map (\(pat, alt) -> (PatLabel pat, (env, stack', alt))) alts )
    _ -> error $ "Error: split var: " ++ show s
  Con tag args -> case stack of
    [] -> (ConNode,
      map (\(i, e)-> (ConLabel $ tag ++ ":" ++ show i, newConf env e)) (zip [1..length args] args)
      )
    _ -> error $ "Spliting with Con and stack: " ++ show stack
