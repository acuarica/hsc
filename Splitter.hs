
module Splitter (Label, split, combine) where

import Expr (Expr(Var, Con, Lam))
import Eval (Conf, StackFrame(Alts), newConf, toExpr)

-- |
-- |
type Label = String

-- | Given a state, returns where to continue the computation.
-- | The given conf must be stucked.
split :: Conf -> [(Label, Conf)]
split s@(env, stack, expr) = case expr of
  Var var -> case stack of
    [] -> []
    Alts alts:stack' -> map (\(pat, alt) -> (var ++ "=" ++ show pat, (env, stack', alt))) alts
    _ -> error $ "Error: split var: " ++ show s
  Con tag args -> case stack of
    [] -> map (\(i, e)-> (tag ++ ":" ++ show i, newConf env e)) (zip [1..length args] args)
    _ -> error $ "Spliting with Con and stack: " ++ show stack

-- | Combines the expression replacing the alternatives.
combine :: Conf -> [Conf] -> Conf
combine s@(env, stack, expr) ss = case expr of
  Var var -> case stack of
    [] -> if null ss then s else error $ "Non-empty ss" ++ show ss
    Alts alts:stack' -> (env, Alts (zipWith rb alts ss):stack', expr)
    _ -> if null ss
      then s
      else error $ "Error combine: Var/splits: " ++ var ++ show ss
  Con tag args -> case stack of
    [] -> if length args == length ss
      then (env, [], Con tag (map toExpr ss))
      else error $ "Args and ss difer: " ++ show args ++ show ss
    _ -> error $ "Stack not empty with " ++ show expr
  _ -> error $ "Error in combine with: " ++ show s
  where rb (p, _) s = (p, toExpr s)
