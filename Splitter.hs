
module Splitter (
    Node(VarNode, ConNode, CaseNode), Label(PatLabel, ConLabel), split
  ) where

import Expr (Expr(Var, Con, Lam), Pat)
import Eval (Conf, StackFrame(Alts), newConf, toExpr)

-- |
-- |
data Node = VarNode | ConNode | CaseNode deriving Show

-- |
-- |
data Label = PatLabel Pat | ConLabel String

instance Show Label where
  show (PatLabel pat) = show pat
  show (ConLabel s) = s

-- | Given a state, returns where to continue the computation.
-- | The given conf must be stucked.
split :: Conf -> (Node, [(Label, Conf)])
split s@(env, stack, expr) = case expr of
  Var var -> case stack of
    [] -> (VarNode, [])
    Alts alts:stack' -> (CaseNode,
      map (\(pat, alt) -> (PatLabel pat, (env, stack', alt))) alts )
    _ -> error $ "Error: split var: " ++ show s
  Con tag args -> case stack of
    [] -> (ConNode,
      map (\(i, e)-> (ConLabel $ tag ++ ":" ++ show i, newConf env e)) (zip [1..length args] args)
      )
    _ -> error $ "Spliting with Con and stack: " ++ show stack
