
module Splitter (split, rebuild) where

import Expr
import Eval

-- | Given a state, returns where to continue the computation.
split :: State -> [State]
split s@(env, stack, expr) = case expr of
  Var var -> case stack of
    [] -> []
    Alts alts:stack' -> map (\(pat, alt) -> (env, stack', alt)) alts
    _ -> error $ "split var " ++ show expr ++ show stack
  Con tag args -> case stack of
    [] -> map (newState env) args
    _ -> error $ "Spliting with Con and stack: " ++ show stack
  _ -> []

-- | Rebuilds the expression replacing the alternatives.
rebuild :: State -> [State] -> State
rebuild s@(env, stack, expr) ss = case expr of
  Var var -> case stack of
    [] -> if null ss then s else error $ "Non-empty ss" ++ show ss
    Alts alts:stack' -> (env, Alts (zipWith rb alts ss):stack', expr)
  Con tag args -> case stack of
    [] -> if length args == length ss
      then (env, stack, Con tag (map toExpr ss))
      else error $ "Args and ss difer: " ++ show args ++ show ss
    _ -> error $ "Stack not empty with " ++ show expr
  _ -> case ss of
    [] -> s
    xs ->  error $ "Error in rebuild with: " ++ show expr
  where rb (p, _) s = (p, toExpr s)
