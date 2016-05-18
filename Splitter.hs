
module Splitter (split, combine) where

import Expr (Expr(Var, Con, Lam))
import Eval (Conf, StackFrame(..), newConf, toExpr)

-- | Given a state, returns where to continue the computation.
-- | It must be Stucked.
split :: Conf -> [Conf]
split s@(env, stack, expr) = case expr of
  Var var -> case stack of
    [] -> []
    Alts alts:stack' -> map (\(pat, alt) -> (env, stack', alt)) alts
    --Arg _:_ -> []
    _ -> error $ "Error: split var: " ++ show s
  Con tag args -> case stack of
    [] -> map (newConf env) args
    _ -> error $ "Spliting with Con and stack: " ++ show stack
  -- Lam var lamexpr -> case stack of
  --   [] -> [(env, [], lamexpr)]
  --   _ -> error $ "Spliting Lam and stack: " ++ show stack
  -- _ -> error $ "Error spliting: " ++ show s

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
  -- Lam var _ -> case stack of
  --   [] -> if length ss == 1
  --     then (env, [], Lam var (toExpr (head ss)))
  --     else error $ "Ss length not 1: " ++ show ss
  --   _ -> error $ "Error: lambda and stack not empty: " ++ show s
  _ -> error $ "Error in combine with: " ++ show s

  where rb (p, _) s = (p, toExpr s)
