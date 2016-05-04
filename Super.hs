
module Super where

import Expr

super :: Expr -> Expr
super = selExpr . fold . (,,,) [] [] 0

type State = (Env, Stack, Time, Expr)

-- | Environment that binds variables to values.
type Env = [(Var, Expr)]

-- | Stack for application calls.
type Stack = [Expr]

-- | Times
type Time = Int

selExpr :: State -> Expr
selExpr (_, _, _, expr) = expr

fold :: State -> State
fold (env, stack, time, expr) = case expr of
  Var var tainted -> (env, stack, time, Var var tainted)
  Con tag args -> (env, stack, time, Con tag (args ++ stack))
  Lam var lamexpr -> (env, stack, time, Lam var lamexpr)
  Let var valexpr inexpr -> (env, stack, time, Let var valexpr inexpr)
  App funexpr valexpr -> (env, stack, newtime, App funexpr valexpr)
  where newtime = time + 1
  --Case scexpr cases ->
