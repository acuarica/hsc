
module Super where

import Data.List
import Control.Arrow
import Debug.Trace

import Expr
import Pretty

supercompile :: Expr -> Expr
supercompile expr = envToLet hist (selExpr state)
  where (hist, state) = reduce 0 [] (newstate expr)

newstate :: Expr -> State
newstate = (,,) [] []

showEnv :: [(Var, Expr)] -> String
showEnv env = intercalate "\n" (map (\(v,e)->v ++ " |-> " ++show e) env)

showRed :: (Hist, State) -> String
showRed (hist, (env, stack, expr)) =
  showEnv hist ++ "\n\n" ++
  showEnv env ++ "\n" ++ show stack ++ "\n" ++ show expr

type State = (Env, Stack, Expr)

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
    top:rest -> Just (env, rest, subst var top lamexpr)
  Let var valexpr inexpr ->
    Just ((var, valexpr):env, stack, inexpr)
  App funexpr valexpr ->
    Just (env, valexpr:stack, funexpr)
  Case scexpr cases _ -> case scexpr of
    Con tag args -> Just (stepCase env stack tag args cases)
    _ -> Nothing

subst :: Var -> Expr -> Expr -> Expr
subst var valexpr bodyexpr = case bodyexpr of
  Var var' t -> if var' == var then valexpr else Var var' t
  _ -> apply (subst var valexpr) bodyexpr

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
  then --trace (show expr ++ "~" ++ show expr' ++" with "++var) $
    (Just var)
  else lookupMatch hist expr

reduce :: Int -> Hist -> State -> (Hist, State)
reduce n hist state@(env,st,ex) = --trace (show state) $
  case lookupMatch hist ex of
  Nothing ->
    case step state of
    Nothing -> case ex of--trace ("reduce: " ++ show ex ++ "@"++show st) ex of
      Var v t -> (hist, (env, st, foldl App (Var v t) st))
      Con tag args -> case reduces args n hist env of
        (args', h') -> ((var n, Con tag args'):h', (env, st, Con tag args'))
      Case scexpr cases t -> case reduces (map snd cases) n hist env of
        (cs', h') -> (
               (var n, Case scexpr (zip (map fst cases) cs') t ):h',
             (env, st, Case scexpr (zip (map fst cases) cs') t ))
      _ -> error $ "Error with Split in: " ++ show ex
    --Just state' -> reduce (n+1) ((var n, selExpr state):hist) state'
    Just state' -> reduce (n+1) ((var n, selExpr state):hist) state'

  Just var -> (hist, replState (Var var False) state)
  --Just var -> reduce n hist (replState (Var var False) state)
  where var n = "$v" ++ show n

reduces :: [Expr] -> Int -> Hist -> Env -> ([Expr], Hist)
reduces [] n hist env = ([], hist)
reduces (x:xs) n hist env = (selExpr s:xs', h')
  where (h, s) = reduce (n+1) hist (env, [], x)
        (xs', h') = reduces xs (n+10) h env
        --var n = "#w_" ++ show n

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

replState :: Expr -> State -> State
replState expr (env, stack, _) = (env, stack, expr)
