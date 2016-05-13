
module Supercompile where

import Data.List
import Control.Arrow
import Debug.Trace
import Data.Maybe (isNothing, fromJust)

import Expr
import Eval
import Splitter

--supercompile :: Expr -> Expr
--supercompile expr = newState []

-- showRed :: (Hist, State) -> String
-- showRed (hist, (env, stack, expr)) =
--   showEnv hist ++ "\n\n" ++
--   showEnv env ++ "\n" ++ show stack ++ "\n" ++ show expr

-- a :: MemoM State -> Expr
-- a (m)

--f $ rebuild s $ mapM memo (split s)
memo :: MemoM State -> MemoM State
memo mstate = do
  state@(env, stack, expr) <- mstate
  let rstate = f state
  ii <- isin rstate
  if isNothing ii
    then do
      rec rstate
      splits <- mapM (memo . return) (split rstate)
      let r = f $ rebuild rstate splits
      --rec r
      return r
    else do
      let var = fromJust ii
      --return rstate
      return (env, stack, Var var)
  where f = reduce

newtype Memo s a = Memo { run :: s -> (a, s) }

instance Functor (Memo s) where
  fmap f (Memo m) = Memo (\s -> let (a, s') = m s in (f a, s))

instance Applicative (Memo s) where
  pure a = Memo (\s -> (a, s))
  f <*> x = error "Applicative Memo not defined"

instance Monad (Memo s) where
  return = pure
  (Memo m) >>= f = Memo (\s ->
    let (a, s') = m s in
    let Memo m' = f a in m' s' )

-- instance (Show s, Show a) => Show (Memo s a) where
--   show (Memo m) = "Count:" ++ show c ++ " ~~ Hist:" ++
--     show hs ++ show x

type MemoM a = Memo (Int, [(Var, Expr)]) a

rec :: State -> MemoM State
rec state = Memo (\(c, env) -> (state, (c+1, (var c, toExpr state):env)))
  where var n = "$v" ++ "_" ++ show n

isin :: State -> MemoM (Maybe Var)
isin state = Memo (
    \(c, env) -> (env `lookupMatch` toExpr state, (c, env))
  )

--emptyHist :: Hist
--emptyHist = (0, [])

match :: Expr -> Expr -> Bool
match expr expr' = expr == expr'

lookupMatch :: Env -> Expr -> Maybe Var
lookupMatch [] _ = Nothing
lookupMatch ((var, expr'):hist) expr = if expr `match` expr'
  then Just var
  else lookupMatch hist expr

-- envToLet :: Hist -> Expr -> Expr
-- envToLet [] expr = expr
-- envToLet ((var,valexpr):env) expr = Let var valexpr (envToLet env expr)

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
