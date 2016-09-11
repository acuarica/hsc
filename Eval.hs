{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Eval (
  Conf, Env, StackFrame(..),
  eval, whnf, emptyEnv, newConf, toExpr, nf, reduce, put, step
) where

import Data.List (intercalate, delete)

import Expr (Expr(..), Var, Pat(Pat), subst, substAlts, lookupAlt, freeVars, alpha)

-- | Represents the configuration of the abstract machine.
type Conf = (Env, Stack, Expr)

-- | Environment that binds variables to values.
type Env = [(Var, Expr)]

-- | Stack for application calls.
type Stack = [StackFrame]

-- Stack frame for stack.
data StackFrame
  = Arg Expr
  | Alts [(Pat, Expr)]
  | Update Var
  deriving Eq

-- | Evaluates the given expression to Normal Form (NF).
-- | It uses alpha to avoid name capture.
eval :: Expr -> Expr
eval = toExpr . nf . newConf emptyEnv -- . alpha

-- | Evaluates the given expression to Weak Head Normal Form (WHNF).
whnf :: Expr -> Expr
whnf = toExpr . reduce . newConf emptyEnv

-- | Creates an empty environment.
emptyEnv :: Env
emptyEnv = []

-- | Given an environment,
-- | creates a new configuration with an empty stack.
newConf :: Env -> Expr -> Conf
newConf env = (,,) env []

-- | Selects the expr from a given configuration:
-- | That is, it uses the expr and the stack.
-- | It does not use the environment.
toExpr :: Conf -> Expr
toExpr conf@(env, stack, expr) = go expr stack
  where go expr [] = expr
        go expr (Arg arg:stack') = go (App expr arg) stack'
        go expr (Alts alts:stack') = go (Case expr alts) stack'
        --go _ _ = error $ "toExpr: " ++ show conf
        go expr (Update var:stack') = go (Let var expr (Var var)) stack'

-- | Reduce a state to Normal Form (NF).
-- | A normal form is either a constructor (Con) or
-- | lambda abstraction (Lam).
nf :: Conf -> Conf
nf state = case reduce state of
  (env, [], Con tag args) ->
    (env, [], Con tag (map (toExpr . nf . newConf env) args))
  (_, _, Con _ _) -> error "Stack/Con"
  state' -> state'

-- | Reduce a conf. to Weak Head Normal Form (WHNF).
-- | Lambda abstractions are not further evaluated as in
-- | Head Normal Form (HNF).
reduce :: Conf -> Conf
reduce conf = case step conf of
  Nothing -> conf
  Just conf' -> reduce conf'

-- | Puts var bind with expr in the given environment.
put :: Var -> Expr -> Env -> Env
put var expr [] = [(var, expr)]
put var expr ((var',expr'):env) = if var' == var
  then (var,expr):env
  else (var',expr'):put var expr env

-- | Operational semantics with one-step reduction.
step :: Conf -> Maybe Conf
step (env, stack, expr) = case expr of
  Var var -> case lookup var env of
    Nothing -> Nothing
    Just val -> Just (env, Update var:stack, val)
  val@(Con tag args) -> case stack of
    [] -> Nothing
    Alts alts:stack' ->
      let (Pat _ patvars, altexpr) = lookupAlt tag alts
      in Just (env, stack', substAlts (zip patvars args) altexpr)
    Update x:stack' -> Just (put x val env, stack', val)
    Arg argexpr:stack' ->
      Just (env, stack', Con tag (args ++ [argexpr]))
  val@(Lam var lamexpr) -> case stack of
    [] -> Nothing
    Arg argexpr:stack' -> Just (env, stack', subst (var, argexpr) lamexpr)
    Update x:stack' -> Just (put x val env, stack', val)
  Let var valexpr inexpr ->
    Just (put var valexpr env, stack, inexpr)
  App funexpr valexpr ->
    Just (env, Arg valexpr:stack, funexpr)
  Case scexpr alts -> Just (env, Alts alts:stack, scexpr)

instance Show StackFrame where
  show frame = case frame of
    Alts alts -> "Alts:" ++ show alts
    Arg expr -> "#(" ++ show expr ++ ")"
    Update var -> "Update:" ++ var

instance {-# OVERLAPPING #-} Show Conf where
  show (env, stack, expr) =
    "<" ++ show env ++ " |" ++ show stack ++ " | " ++ show expr ++ " >"

instance {-# OVERLAPPING #-} Show (Stack, Expr) where
  show (stack, expr) =
    "<%" ++ show stack ++ " | " ++ show expr ++ " %>"

instance {-# OVERLAPPING #-} Show Env where
  show env = intercalate " &" (map ((++) " " . show) env)

instance {-# OVERLAPPING #-} Show (Var, Expr) where
  show (var, expr) = var -- ++ "=" ++ show expr

instance {-# OVERLAPPING #-} Show Stack where
  show stack = intercalate "|" (map ((++) " " . show) stack)
