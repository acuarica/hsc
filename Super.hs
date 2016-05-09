
module Super where

import Control.Arrow

import Expr
import Pretty

supercompile :: Expr -> Expr
supercompile expr = envToLet hist (selExpr state)
  where (hist, state) = reduce 0 [] (newstate expr)

newstate :: Expr -> State
newstate = (,,) [] []

--data Op = Push | Put Var deriving (Eq, Show)
type State = (Env, Stack, Expr)

-- doOp :: State -> State
-- doOp (env, stack, (expr, op):queue) = case op of
--   Push -> (env, expr:stack, queue)
--   Put var -> ((var, expr):env, stack, queue)

step :: State -> Maybe State
step (env, stack, expr) = case expr of
  Var var tainted -> case lookup var env of
    Nothing -> Nothing
    Just val -> Just (env, stack, val)
  Con tag args -> case stack of
    [] -> Nothing
    stack -> Just (env, [], Con tag (args ++ stack))
  Lam var lamexpr -> case stack of
    [] -> Nothing
    top:rest -> Just ((var, top):env, rest, lamexpr)
  Let var valexpr inexpr ->
    Just ((var, valexpr):env, stack, inexpr)
  App funexpr valexpr ->
    Just (env, valexpr:stack, funexpr)
  Case scexpr cases _ -> case scexpr of
    Con tag args -> Just (stepCase env stack tag args cases)
    _ -> Nothing

stepCase :: Env -> Stack -> Tag -> [Expr] -> [(Pat, Expr)] -> State
stepCase env stack tag args cases = (altEnv args patvars env, stack, expr)
  where (Pat _ patvars, expr) = lookupCase tag cases

lookupCase :: Tag -> [(Pat, Expr)] -> (Pat, Expr)
lookupCase tag cases = case cases of
  (Pat pattag patvars, expr):cases' -> if pattag == tag
    then (Pat pattag patvars, expr)
    else lookupCase tag cases'

altEnv :: [Expr] -> [Var] -> Env -> Env
altEnv scargs patvars env = case (scargs, patvars) of
  ([], []) -> env
  (scarg':scargs', patvar':patvars') ->
    altEnv scargs' patvars' ((patvar', scarg'):env)
  _ -> error "Incorrect matching case"

type Hist = [(Var, Expr)]

match :: Expr -> Expr -> Bool
match expr expr' = expr == expr'

lookupMatch :: Hist -> Expr -> Maybe Var
lookupMatch [] _ = Nothing
lookupMatch ((var, expr'):hist) expr = if match expr expr'
  then Just var
  else lookupMatch hist expr

replState :: Expr -> State -> State
replState expr (env, stack, _) = (env, stack, expr)

reduce :: Int -> Hist -> State -> (Hist, State)
reduce n hist state = case lookupMatch hist (selExpr state) of
  Nothing -> case step state of
    --Nothing -> ((var n, selExpr state):hist, state)
    Nothing -> (hist, replState (split go (selExpr state)) state)
    Just state' -> reduce (n+1) ((var n, selExpr state):hist) state'
  Just var -> (hist, replState (Var var False) state)
  where var n = "$v_" ++ show n
        go expr = selExpr $ snd $ reduce (n+1) hist (replState expr state)

split :: (Expr -> Expr) -> Expr -> Expr
split f expr = case expr of
  Var var t -> Var var t
  Con tag args -> Con tag (map f args)
  Case scexpr cases t -> Case scexpr (map (second f) cases) t
  e' -> error $ "Error with Split in: " ++ show e'

envToLet :: Hist -> Expr -> Expr
envToLet [] expr = expr
envToLet ((var,valexpr):env) expr = Let var valexpr (envToLet env expr)

-- | Environment that binds variables to values.
type Env = [(Var, Expr)]

-- | Stack for application calls.
type Stack = [Expr]

-- | Times
type Time = Int

selExpr :: State -> Expr
selExpr (_, _, expr) = expr

fold' :: State -> State
fold' (env, stack, expr) = case expr of
  Var var tainted ->
    (env, stack, Var var tainted)
  Con tag args -> (env, stack, Con tag (args ++ stack))
  Lam var lamexpr -> (env, stack, Lam var lamexpr)
  Let var valexpr inexpr -> (env, stack, Let var valexpr inexpr)
  App funexpr valexpr ->
    (env, stack, App
      (selExpr (fold' (env, stack, funexpr)))
      (selExpr (fold' (env, stack, valexpr)))
      )
  --where newtime = time + 1
  --Case scexpr cases ->
