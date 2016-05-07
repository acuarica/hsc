
module Super where

import Expr

supercompile :: Expr -> Expr
supercompile = selExpr . step . newstate

newstate :: Expr -> State
newstate expr = ([], [], [(expr, Push)])

data Op = Push | Put Var deriving (Eq, Show)
type State = (Env, Stack, [(Expr, Op)])

doOp :: State -> State
doOp (env, stack, (expr, op):queue) = case op of
  Push -> (env, expr:stack, queue)
  Put var -> ((var, expr):env, stack, queue)

step :: State -> State
step (env, stack, (expr, op):queue) = case expr of
  Var var tainted -> case lookup var env of
    Nothing -> (env, stack, (Var var tainted, op):queue)
    Just val -> (env, stack, (val, op):queue)
  Con tag [] -> case stack of
    [] -> doOp (env, [], (Con tag stack, op):queue)
  Let var valexpr inexpr ->
    (env, stack, (valexpr, Put var):(inexpr, op):queue)
  App funexpr valexpr ->
    (env, stack, (valexpr, Push):(funexpr, op):queue)

--
-- reduce :: State -> State
-- reduce state = case step state of
--   Nothing -> state
--   Just s' -> reduce s'

-- | Environment that binds variables to values.
type Env = [(Var, Expr)]

-- | Stack for application calls.
type Stack = [Expr]

-- | Times
type Time = Int

selExpr :: State -> Expr
selExpr (_, _, (expr, _):_) = expr

-- fold :: State -> State
-- fold (env, stack, expr) = case expr of
--   Var var tainted -> (env, stack, Var var tainted)
--   Con tag args -> (env, stack, Con tag (args ++ stack))
--   Lam var lamexpr -> (env, stack, Lam var lamexpr)
--   Let var valexpr inexpr -> (env, stack, Let var valexpr inexpr)
--   App funexpr valexpr -> (env, stack, App funexpr valexpr)
--   --where newtime = time + 1
--   --Case scexpr cases ->
