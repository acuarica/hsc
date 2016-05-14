
module Supercompile where

import Data.List
import Control.Arrow
import Debug.Trace
import Data.Maybe (isNothing, fromJust)

import Expr
import Eval
import Splitter

supercompile :: Expr -> Expr
supercompile expr = let (c, (count, hist)) = runMemo expr
  in fromMemo hist (toExpr c)

--fromMemo (c, s) = toExpr c
--runMemo :: Expr -> Memo Conf
runMemo expr = let s = memo (return (newConf emptyEnv expr))
  in run s (0, [])

fromMemo :: [(Var, Expr)] -> Expr -> Expr
fromMemo [] expr = expr
fromMemo ((var,valexpr):env) expr = Let var valexpr (fromMemo env expr)

memo :: Memo Conf -> Memo Conf
memo mstate = do
  state@(env, stack, expr) <- mstate
  let rstate = f state
  ii <- isin rstate
  if isNothing ii
    then do
      rec rstate
      splits <- mapM (memo . return) (split rstate)
      --let r = f $ combine rstate splits
      let r = combine rstate splits
      return r
    else do
      let var = fromJust ii
      --return rstate
      return (env, stack, Var var)
  where f = reduce

-- | State monad.
newtype State s a = State { run :: s -> (a, s) }

instance Functor (State s) where
  fmap f (State m) = State (\s -> let (a, s') = m s in (f a, s))

instance Applicative (State s) where
  pure a = State (\s -> (a, s))
  f <*> x = error "Applicative Memo not defined"

instance Monad (State s) where
  return = pure
  (State m) >>= f = State (\s ->
    let (a, s') = m s in
    let State m' = f a in m' s' )

type Hist = [(Var, Conf)]

type Memo a = State (Int, Hist) a

rec :: Conf -> Memo Conf
rec conf = State (\(c, env) -> (conf, (c+1, (var c, conf):env)))
  where var n = "$v" ++ "_" ++ show n

isin :: Conf -> Memo (Maybe Var)
isin conf = State (
    \(c, env) -> (env `lookupMatch` conf, (c, env))
  )

match :: Expr -> Expr -> Bool
match expr expr' = expr == expr'

lookupMatch :: Env -> Expr -> Maybe Var
lookupMatch [] _ = Nothing
lookupMatch ((var, expr'):hist) expr = if expr `match` expr'
  then Just var
  else lookupMatch hist expr

type Fresh = Int

aform' :: (Fresh, Env, Expr) -> (Fresh, Env, Expr)
aform' (fr, env, expr) = case expr of
  Var var -> (fr, env, expr)
  Lam var lamexpr -> case aform' (fr, env, lamexpr) of
    (fr', env', lamexpr') -> (fr', env', Lam var lamexpr')
  App funexpr valexpr -> case aform' (fr, env, valexpr) of
    (fr', env', valexpr') -> case aform' (fr', env', funexpr) of
      (fr'', env'', funexpr'') ->
        (fr''+2,
          (make (fr'' + 1), App funexpr'' (Var (make fr''))):
          (make fr'', valexpr'):env'',
          Var (make (fr''+1)) )
    --Let (make fr) (apply (aform (fr+1)) valexpr)
      --((makefr, valexpr) :env)
      --(App funexpr (usevar (make fr)))
    --expr -> apply (aform (fr+1)) expr
  where make i = "$v" ++ "_" ++ show i
      --  newfr = fr + 1

-- aform :: Expr -> Expr
-- aform expr = case aform' (0, [], expr) of
--   (fr', env', expr') -> envToLet env' expr'
