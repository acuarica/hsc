
module Super where

import Control.Arrow

import Debug.Trace

import Expr
import Pretty

supercompile :: Expr -> Expr
supercompile expr = envToLet hist (selExpr state)
  where (hist, state) = reduce 0 [] (newstate expr)

newstate :: Expr -> State
newstate = (,,) [] []

type State = (Env, Stack, Expr)

subst :: Var -> Expr -> Expr -> Expr
subst var valexpr bodyexpr = case bodyexpr of
  Var var' t -> if var' == var then valexpr else Var var' t
  _ -> apply (subst var valexpr) bodyexpr

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
  then trace (show expr ++ "~" ++ show expr' ++" with "++var) (Just var)
  else lookupMatch hist expr

replState :: Expr -> State -> State
replState expr (env, stack, _) = (env, stack, expr)

newtype HistM a = HistM [(Var, a)]

instance Functor HistM where
  fmap f (HistM xs) = HistM (map (second f) xs)

instance Applicative HistM where
  pure a = HistM []
  (HistM f) <*> (HistM xs) = error ""

--instance Monad HistM where
  --return = pure
  --m a -> (a -> m b) -> m b
  --HistM xs >>= f = HistM

reduce :: Int -> Hist -> State -> (Hist, State)
reduce n hist state@(env,st,ex) = traceShow state $
  case lookupMatch hist ex of
  Nothing -> case step state of
    Nothing -> case trace ("reduce: " ++ show ex ++ "@"++show st) ex of
      Var v t -> (hist, (env, st, foldl App (Var v t) st))
      Con tag args -> case reduces args n hist env of
        (args', h') -> ((var n, Con tag args'):h', (env, st, Con tag args'))
      Case scexpr cases t -> case reduces (map snd cases) n hist env of
        (cs', h') -> (
               (var n, Case scexpr (zip (map fst cases) cs') t ):h',
             (env, st, Case scexpr (zip (map fst cases) cs') t ))
      _ -> error $ "Error with Split in: " ++ show ex
    Just state' -> reduce (n+1) ((var n, selExpr state):hist) state'
  --Just var -> (hist, replState (Var var False) state)
  Just var -> reduce n hist (replState (Var var False) state)
  where var n = "$v_" ++ show n
--
-- deapply :: Stack -> Expr -> Expr
-- deapply stack expr = case stack of
--   [] -> expr
--   top:rest -> deapply rest (App expr top)

reduces :: [Expr] -> Int -> Hist -> Env -> ([Expr], Hist)
reduces [] n hist env = ([], hist)
reduces (x:xs) n hist env = (selExpr s:xs', h')
  where (h, s) = reduce (n+1) hist (env, [], x)
        (xs', h') = reduces xs (n+10) ((var n, selExpr s):h) env
        var n = "#w_" ++ show n

data C a = C Int a

instance Functor C where
  fmap f (C n a) = C n (f a)

instance Applicative C where
  pure = return
  (C n f) <*> (C n' a) = C (n+n') (f a)

instance Monad C where
  return = C 0
  (C n expr) >>= f = case f expr of
    C n' expr' -> C (n'+n) expr'



sp :: Monad m => (Expr -> m Expr) -> m Expr -> m Expr
sp f mexpr = do
  expr <- mexpr
  case expr of
    Var var t -> return (Var var t)
    Lam v lexpr -> f lexpr >>= (\a -> return (Lam v a))
  return expr
      --return ()
    --Con tag args -> Con tag (mapM f args)

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
