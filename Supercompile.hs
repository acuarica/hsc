{-# LANGUAGE FlexibleInstances #-}

module Supercompile where

import Data.List
--import Control.Arrow
import Debug.Trace
import Data.Maybe (isNothing, fromJust)

import Expr
import Eval
import Splitter

supercompile :: Expr -> Expr
supercompile = gp . runMemo

gp m@(conf, (next, hist, prom)) =
  let (var, fv, expr) = head prom in
    fromMemo prom (app (Var var) (map Var fv))

runMemo expr = let s = memo (newConf emptyEnv expr)
  in run s (0, [], [])

fromMemo :: [(Var, [Var], Conf)] -> Expr -> Expr
fromMemo [] expr = expr
fromMemo ((var, fv, valconf):env) expr =
    Let (var) (toLambda fv (toExpr valconf)) (fromMemo env expr)

toLambda :: [Var] -> Expr -> Expr
toLambda [] expr = expr
toLambda (v:vs) expr = Lam v (toLambda vs expr)

add :: Var -> Expr -> Conf -> Conf
add var valexpr (env, stack, expr) = (put var valexpr env, stack, expr)

memo :: Conf -> Memo Conf
memo state@(env, stack, expr) = do
  ii <- isin state
  if isNothing ii
    then do
      (v, fv) <- rec state
      let rstate = reduce state
      splits <- mapM memo (split rstate)
      let r@(env', stack', expr') = combine rstate splits
      promise v fv r
      return (env', stack', app (Var v) (map Var fv))
    else do
      let (var, fv) = fromJust ii
      return (env, stack, app (Var var) (map Var fv))

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

type Hist = [(Var, [Var], Conf)]

type Prom = [(Var, [Var], Conf)]

instance {-# OVERLAPPING #-} Show Hist where
  show hist = "" ++
    intercalate "\n" (map ((++) "  " . show) hist)

instance {-# OVERLAPPING #-} Show (Var, [Var], Conf) where
  show (var, args, expr) =
    var ++ "(" ++ unwords args ++ ") ~> " ++ show expr

-- instance {-# OVERLAPPING #-} Show Prom where
--   show env = "" ++
--     intercalate "\n" (map ((++) "  " . show) env)

instance {-# OVERLAPPING #-} Show (Var, Conf) where
  show (var, expr) = var ++ " ~> " ++ show expr

instance {-# OVERLAPPING #-} Show a => Show (a, (Int, Hist, Prom)) where
  show (val, (next, hist, prom)) =
    "Value: " ++ show val ++ "\n" ++
    "Next: " ++ show next ++ "\n" ++
    "Hist: \n" ++ show hist ++ "\n" ++
    "Prom: \n" ++ show prom


type Memo a = State (Int, Hist, Prom) a

-- instance {-# OVERLAPPING #-} Show (Int, Conf) where
--   show (var, expr) = var ++ " ~> " ++ show expr

envExpr :: Conf -> Expr
envExpr conf@(env, stack, expr) = rebuildEnv env (toExpr conf)
  where rebuildEnv [] expr = expr
        rebuildEnv ((var,valexpr):env) expr =
          Let var valexpr (rebuildEnv env expr)

rec :: Conf -> Memo (Var, [Var])
rec conf = State (\(next, hist, prom) ->
  let var = "$v_" ++ show next in
  let fv = (freeVars . envExpr) conf in
    ((var, fv), (next+1, (var, fv, conf):hist, prom))
  )

promise :: Var -> [Var] -> Conf -> Memo ()
promise var fv conf = State (\(next, hist, prom) -> ((),
    (next, hist, (var, fv, conf):prom))
  )

isin :: Conf -> Memo (Maybe (Var, [Var]))
isin conf = State (
    \(next, hist, prom) -> (hist `lookupMatch` conf, (next, hist, prom))
  )

match :: Conf -> Conf -> Bool
match conf conf' = toExpr (reduce conf) == toExpr (reduce conf')

lookupMatch :: Hist -> Conf -> Maybe (Var, [Var])
lookupMatch [] _ = Nothing
lookupMatch ((var, vars, conf'):hist) conf = if conf `match` conf'
  then Just (var, vars)
  else lookupMatch hist conf
