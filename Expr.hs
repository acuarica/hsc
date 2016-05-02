module Expr where

--import Data.Maybe (fromJust)

import Debug.Trace

-- | Represents identifier variable.
type Var = String

-- | Represents constructor name.
-- | Also called tag to be matched in case expressions.
type Tag = String

-- | The expression type.
data Expr
  = Var  Var Bool
  | Con  Tag [Value]
  | Let  Var Expr Expr
  | Lam  Var Expr
  | App  Expr Expr
  | Case Expr [(Pat, Expr)]
  deriving (Eq, Show)

-- | Represents patterns in case expressions.
type Pat = Expr

-- | The Value type represents the result of a computation.
-- | For now, let's use Expr.
type Value = Expr

-- | Stack for application calls.
type Stack = [() -> Value]

-- | Internal eval.
eval' :: Env -> Stack -> Int -> Expr -> Value
eval' env stack t expr =
  --trace (show t ++ " -- " ++ show (map (\(x,y)->(x,pprint y)) env) ++
    --show (map pprint stack) ++ pprint expr) $
 --if t > 20 then expr else
   case expr of
  Var var tainted -> --if tainted
    --then Var var False
    --else case
    case fetch var env of
      Nothing -> Var var True
      Just val -> eval' env stack nt val
  Con tag args -> Con tag (--map (eval' env [] nt) args ++
                           args ++ map (\v -> v () ) stack)
  Lam var lamexpr -> case stack of
      [] -> Lam var (eval' (taintVar var env) [] nt lamexpr)
      valexpr:rest ->
        eval' (put var (\()->Just (valexpr () )) env) rest nt lamexpr
  Let var valexpr inexpr ->
    eval' (put var (\()->Just valexpr) env) stack nt inexpr
  App funexpr valexpr ->
    --eval' env ((eval' env [] nt valexpr, env) :stack) nt funexpr
    eval' env ((\()->eval' env [] nt valexpr) :stack) nt funexpr
  c@(Case sexpr cases) -> case eval' env stack nt sexpr of
    Con tag args -> evalAlt env tag args cases nt
    _ -> c
  where nt = t + 1

-- | Environment that binds variables to values.
type Env = Var -> (() -> Maybe Expr)

put :: Var -> (() -> Maybe Expr) -> Env -> Env
put var expr env = \v -> if v == var then expr else env v
-- put var expr env = \v-> case env v of
--   Nothing -> if v==var then Just expr else Nothing
--   Just expr' -> Just expr'

-- merge :: Env -> Env -> Env
-- merge a b = \v -> case a v of
--   Nothing -> b v
--   res -> res

-- | Removes the variable var binding from the environment.
taintVar :: Var -> Env -> Env
taintVar var env = --trace ("Taiting " ++ var) $
  (\v -> --trace (var ++ " tainted while looking for " ++ v) $
    if v == var then (\_->Nothing) else env v)

newenv :: Env
newenv = \v -> const Nothing

fetch :: Var -> Env -> Maybe Expr
fetch var env = --trace ("Fetching " ++ var) $
  env var ()

-- | Gets the right case alternative.
evalAlt :: Env -> Tag -> [Value] -> [(Pat, Expr)] -> Int -> Expr
evalAlt env sctag scargs pats times = case pats of
  [] -> error ("Constructor tag not found: " ++ sctag)
  ((patexpr, altexpr):pats') -> case eval patexpr of
    Con pattag patargs -> if pattag == sctag
      then eval' (buildAltEnv scargs patargs env) [] times altexpr
      else evalAlt env sctag scargs pats' times
    _ -> evalAlt env sctag scargs pats' times

buildAltEnv :: [Value] -> [Value] -> Env -> Env
buildAltEnv scargs patargs env = case (scargs, patargs) of
  ([], []) -> env
  (scarg':scargs', patarg':patargs') -> case patarg' of
    Var var _ -> buildAltEnv scargs' patargs' (put var (\_->Just scarg') env)
    _ -> buildAltEnv scargs' patargs' env
  _ -> error "Incorrect matching case"

-- | The eval function.
eval :: Expr -> Value
eval = eval' newenv [] 0
