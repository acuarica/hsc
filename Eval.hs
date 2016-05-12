{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Eval where

import Data.List (intercalate)

import Expr

-- | The eval function with head normal form reduction.
eval :: Expr -> Expr
eval = toExpr . hnf . newState []

-- | State of the eval machine.
type State = (Env, Stack, Expr)

-- | Creates a new state with an empty stack.
newState :: Env -> Expr -> State
newState env = (,,) env []

-- | Selects the expr from a given state: That is, it uses the expr
-- | and the stack.
toExpr :: State -> Expr
toExpr s@(env, stack, expr) = go expr stack
  where go expr [] = expr
        go expr (Arg arg:stack') = go (App expr arg) stack'
        go expr (Alts alts:stack') = go (Case expr alts) stack'
        go _ _ = error $ "toExpr error: " ++ show s

-- | Environment that binds variables to values.
type Env = [(Var, Expr)]

-- | Stack for application calls.
type Stack = [StackFrame]

-- Stack frame for stack.
data StackFrame
  = Alts [(Pat, Expr)]
  | Arg Expr
  | Update Var
  deriving Eq

instance Show StackFrame where
  show frame = case frame of
    Alts alts -> "Alts:" ++ show alts
    Arg expr -> '#':show expr
    Update var -> "Update:" ++ var

instance {-# OVERLAPPING #-} Show State where
  show (env, stack, expr) =
    "Env:\n" ++ show env ++ "\n" ++
    "Stack:\n" ++ show stack ++ "\n" ++
    "Expr: " ++ show expr

instance {-# OVERLAPPING #-} Show Env where
  show env = intercalate "\n" (map ((++) "  " . show) env)

instance {-# OVERLAPPING #-} Show (Var, Expr) where
  show (var, expr) = var ++ " |-> " ++ show expr

instance {-# OVERLAPPING #-} Show Stack where
  show stack = intercalate "\n" (map ((++) "  | " . show) stack)

-- | Reduce a state to Head Normal Form (HNF).
-- | A normal form is either a constructor (Con) or
-- | lambda abstraction (Lam).
hnf :: State -> State
hnf state = case reduce state of
  (env, [], Con tag args) ->
    (env, [], Con tag (map (toExpr . hnf . newState env) args))
  (env, stack, Con _ _) -> error "Stack/Con"
  state' -> state'

-- | Reduce a state to Weak Head Normal Form (WHNF).
reduce :: State -> State
reduce state = case step state of
  Nothing -> state
  Just state' -> reduce state'

-- | Puts var bind with expr in the given environment.
put :: Var -> Expr -> Env -> Env
put var expr [] = [(var, expr)]
put var expr ((var',expr'):env) = if var' == var
  then (var,expr):env
  else (var',expr'):put var expr env

-- | Operational semantics with one-step reduction.
step :: State -> Maybe State
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
