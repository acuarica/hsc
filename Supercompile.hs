{-# LANGUAGE FlexibleInstances #-}

module Supercompile where

import Data.List
import Debug.Trace
import Control.Exception (assert)
import Data.Maybe (isNothing, fromJust)
import Control.Monad (unless, void)
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

envToLet :: Env -> Expr -> Expr
envToLet [] expr = expr
envToLet ((var, valexpr):env) expr = Let var valexpr (envToLet env expr)

add :: Var -> Expr -> Conf -> Conf
add var valexpr (env, stack, expr) = (put var valexpr env, stack, expr)

freduce :: [Expr] -> Conf -> Conf
freduce args conf =
  let (env, stack, expr) = reduce conf in
  case expr of
    Lam var lamexpr ->
      assert (null stack)
        (freduce (tail args) (env, [Arg (head args)], Lam var lamexpr))
    _ -> (env, stack, expr)

spreduce = reduce

memo :: Conf -> Memo Conf
memo state@(env, stack, expr) =
  -- let rstate' = reduce state in
  -- let splits' = split rstate' in
  --trace (show state ++ " ~~>> ") $
  -- traceShow rstate' $
  -- trace ("Splits: " ++ show splits') $
  -- trace ("Combine: " ++ show (combine rstate' splits')) $
  -- trace "" $
  do


  ii <- isin state

  if isNothing ii
    then do
      (v, fv) <- rec $   state

      --let rstate = reduce state
      let rstate = spreduce state

      --rec rstate
      --mapM_ rec (split rstate)
      splits <- mapM memo (split rstate)
      --mapM_ rec splits
      --unless (null splits) $ void (rec (head splits))
        --else return ()

      let r@(env', stack', expr') = combine rstate splits
      promise v fv r
      return (env', stack', appVars (Var v) fv)
    else do
      --(v, fv) <- rec state
      let (var, fv) = fromJust ii
      return (env, stack, appVars (Var var) (fvs state))
  where fvs = freeVars . envExpr

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

instance {-# OVERLAPPING #-} Show (Var, Conf) where
  show (var, expr) = var ++ " ~> " ++ show expr

instance {-# OVERLAPPING #-} Show a => Show (a, (Int, Hist, Prom)) where
  show (val, (next, hist, prom)) =
    "Value: " ++ show val ++ "\n" ++
    "Next: " ++ show next ++ "\n" ++
    "Hist: \n" ++ show hist ++ "\n" ++
    "Prom: \n" ++ show prom

type Memo a = State (Int, Hist, Prom) a

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

-- | Not alpha-equivalence. Free variables equivalence.
-- | Implementation not nice. Support only up to 10 arguments!!!
match :: Conf -> Conf -> Bool
match lhs rhs = toExpr lred == toExpr rred
  where
    lexpr = envExpr lhs
    rexpr = envExpr rhs
    lfv   = freeVars lexpr
    rfv   = freeVars rexpr
    llam  = toLambda lfv lexpr
    rlam  = toLambda rfv rexpr
    lred  = freduce args (newConf emptyEnv llam)
    rred  = freduce args (newConf emptyEnv rlam)
    args  = map (Var . (++) "$a_" . show) [1..10]

lookupMatch :: Hist -> Conf -> Maybe (Var, [Var])
lookupMatch [] _ = Nothing
lookupMatch ((var, vars, conf'):hist) conf = if conf `match` conf'
  then Just (var, vars)
  else lookupMatch hist conf
