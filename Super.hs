
module Super where

import Expr

super :: Expr -> Expr
super expr = fold expr

type State = (Env, Stack, Time, Expr)

-- | Environment that binds variables to values.
type Env = [(Var, Expr)]

-- | Stack for application calls.
type Stack = [Expr]

-- | Times
type Time = Int

selexpr :: State -> Expr
selExpr (_, _, _, expr) = Expr

fold (env, stack, time, expr) = case expr of
  Var var tainted -> Var var tainted
  Con tag args -> (env, stack, time, Con tag (args ++ stack))
  Lam var lamexpr ->
  Let var valexpr inexpr -> Let var valexpr (inexpr)
  App funexpr valexpr -> (env, stack, newtime, App
  where newtime = time + 1
  --Case scexpr cases ->
