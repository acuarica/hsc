
module Supercompile where

import Data.List
--import Control.Arrow
import Debug.Trace
import Data.Maybe (isNothing, fromJust)

import Expr
import Eval
import Splitter

supercompile :: Expr -> Expr
supercompile expr = let (conf, (next, hist, prom)) = runMemo expr
  in fromMemo prom (Var ((fst .head) prom))

runMemo expr = let s = memo (newConf emptyEnv expr)
  in run s (0, [], [])

fromMemo :: [(Var, Conf)] -> Expr -> Expr
fromMemo [] expr = expr
fromMemo ((var,valconf):env) expr =
    Let var (toExpr valconf) (fromMemo env expr)

add :: Var -> Expr -> Conf -> Conf
add var valexpr (env, stack, expr) = (put var valexpr env, stack, expr)

memo :: Conf -> Memo Conf
memo state@(env, stack, expr) = do
  ii <- isin state
  if isNothing ii
    then do
      v <- rec state
      let rstate = reduce state
      splits <- mapM memo (split rstate)
      let r@(env', stack', expr') = combine rstate splits
      promise v r
      return (env', stack', app (Var v) (map Var (freeVars (toExpr state))))
    else do
      let var = fromJust ii
      return (env, stack, Var var)

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

type Prom = [(Var, Conf)]

type Memo a = State (Int, Hist, Prom) a

rec :: Conf -> Memo Var
rec conf = State (\(next, hist, prom) ->
  let var = "$v" ++ "_" ++ show next in
    (var, (next+1, (var, conf):hist, prom))
  )

promise :: Var -> Conf -> Memo ()
promise var conf = State (\(next, hist, prom) -> ((),
    (next, hist, (var, conf):prom))
  )

isin :: Conf -> Memo (Maybe Var)
isin conf = State (
    \(next, hist, prom) -> (hist `lookupMatch` conf, (next, hist, prom))
  )

match :: Conf -> Conf -> Bool
match conf conf' = toExpr (reduce conf) == toExpr (reduce conf')

lookupMatch :: Hist -> Conf -> Maybe Var
lookupMatch [] _ = Nothing
lookupMatch ((var, conf'):hist) conf = if conf `match` conf'
  then Just var
  else lookupMatch hist conf
