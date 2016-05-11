
module Eval where

import Expr

-- | The eval function with head normal form reduction.
eval :: Expr -> Expr
eval = selExpr . hnf . newState []

-- | State of the eval machine.
type State = (Env, Stack, Expr)

-- | Creates a new state with an empty stack.
newState :: Env -> Expr -> State
newState env = (,,) env []

-- | Selects the expr from a given state.
selExpr :: State -> Expr
selExpr state = let (_, _, expr) = state in expr

-- | Environment that binds variables to values.
type Env = [(Var, Expr)]

-- | Stack for application calls.
type Stack = [StackFrame]

-- Stack frame for stack.
data StackFrame
  = Alts [(Pat, Expr)]
  | Arg Expr
  | Update Var

-- | Reduce a state to Head Normal Form (HNF).
-- | A normal form is either a constructor (Con) or
-- | lambda abstraction (Lam).
hnf :: State -> State
hnf state = case reduce state of
  (env, [], Con tag args) ->
    (env, [], Con tag (map (selExpr . hnf . newState env) args))
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
