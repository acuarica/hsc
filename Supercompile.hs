
module Supercompile where

import Data.List
import Control.Arrow
import Debug.Trace

import Expr
import Eval

--supercompile :: Expr -> Expr
--supercompile expr = newState []

-- showRed :: (Hist, State) -> String
-- showRed (hist, (env, stack, expr)) =
--   showEnv hist ++ "\n\n" ++
--   showEnv env ++ "\n" ++ show stack ++ "\n" ++ show expr

--memo :: State -> Sta
--mm = memo 0

memo state = --if hist < 2
  --then
    f $ rebuild s $ map memo (split s)
  --else reduce state
  where s = f state
        f = hnf

-- | Given a state, returns where to continue the computation.
split :: State -> [State]
split s@(env, stack, expr) = case expr of
  Var var -> case stack of
    Alts alts:stack' -> map (\(pat, alt) -> (env, stack', alt)) alts
  _ -> []

-- | Rebuilds the expression replacing the alternatives.
-- | The state has to be stucked.
rebuild :: State -> [State] -> State
rebuild s@(env, stack, expr) ss = case expr of
  Var var -> case stack of
    Alts alts:stack' -> (env, Alts (zipWith rb alts ss):stack', expr)
  _ -> case ss of
    [] -> s
    xs ->  error $ "Error in rebuild with: " ++ show expr-- ++" and "++show ss
  where rb (p, _) s = (p, toExpr s)

type Hist = Int --(Int, [(Var, Expr)])

--emptyHist :: Hist
--emptyHist = (0, [])

match :: Expr -> Expr -> Bool
match expr expr' = expr == expr'
--
-- lookupMatch :: Hist -> Expr -> Maybe Var
-- lookupMatch [] _ = Nothing
-- lookupMatch ((var, expr'):hist) expr = if match expr expr'
--   then Just var
--   else lookupMatch hist expr

--  where var n = "$v" ++ show n

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
