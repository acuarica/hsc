
module Splitter (split, combine) where

import Expr (Expr(Var, Con))
import Eval (Conf, StackFrame(..), newConf, toExpr)

-- | Given a state, returns where to continue the computation.
split :: Conf -> [Conf]
split s@(env, stack, expr) = case expr of
  Var var -> case stack of
    [] -> []
    Alts alts:stack' -> map (\(pat, alt) -> (env, stack', alt)) alts
    Arg _:_ -> []
    _ -> error $ "Error: split var " ++ show expr ++ show stack
  Con tag args -> case stack of
    [] -> map (newConf env) args
    _ -> error $ "Spliting with Con and stack: " ++ show stack
  _ -> []

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
      then (env, stack, Con tag (map toExpr ss))
      else error $ "Args and ss difer: " ++ show args ++ show ss
    _ -> error $ "Stack not empty with " ++ show expr
  _ -> case ss of
    [] -> s
    xs ->  error $ "Error in combine with: " ++ show expr
  where rb (p, _) s = (p, toExpr s)
