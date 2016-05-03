
module Expr where

import Debug.Trace
import Control.Arrow

-- | Represents identifier variable.
type Var = String

-- | Represents constructor name.
-- | Also called tag to be matched in case expressions.
type Tag = String

-- | The expression type.
data Expr
  = Var  Var  Bool
  | Con  Tag  [Expr]
  | Let  Var  Expr Expr
  | Lam  Var  Expr
  | App  Expr Expr
  | Case Expr [(Pat, Expr)]
  deriving (Eq, Show)

-- | Represents patterns in case expressions.
type Pat = Expr

-- | The Value type represents the result of a computation.
-- | For now, let's use Expr.
--type Value = Expr

-- | Stack for application calls.
type Stack = [Expr]

type State = (Env, Stack, Expr)

selexpr :: State -> Expr
selexpr (_, _, expr) = expr
--
-- evalStack :: Env -> (Stack, [Expr]) ->(Stack, [Expr])
-- evalStack env ([], exprs) = ([], exprs)
-- evalStack env (expr:stack, exprs) = case eval' (env, [], expr) of
--   (env', [], expr') -> evalStack env (stack, exprs ++ [expr'])
--   (env', stack', expr') -> (stack, exprs ++ [expr])

untaint :: Expr -> Expr
untaint expr = case expr of
  Var var tainted -> Var var False
  Con tag args -> Con tag (map untaint args)
  Lam var lamexpr -> Lam var (untaint lamexpr)
  Let var valexpr inexpr -> Let var (untaint valexpr) (untaint inexpr)
  App funexpr valexpr -> App (untaint funexpr) (untaint valexpr)
  Case scexpr cases -> Case (untaint scexpr) (map (second untaint) cases)

-- | Internal eval.
eval' :: State -> State
eval' (env, stack, expr) = case expr of
  Var var tainted -> if tainted
    then (env, stack, Var var True)
    else case fetch var env of
      (Nothing,_) -> (taintVar var env, stack, Var var True)
      (Just val,env') -> eval' (env, stack, val)
  Con tag args -> (env, [], Con tag (
    map (selexpr . eval' . (,,) env []) args ++
    map (selexpr . eval' . (,,) env []) stack ))
  Lam var lamexpr -> case stack of
      [] -> case eval' (taintVar var env, [], lamexpr) of
        (env', stack', lamexpr') -> (env, stack', Lam var lamexpr')
      valexpr:rest ->
        case eval' (put var valexpr env, rest, untaint lamexpr) of
        (env', stack', lamexpr') ->
          (env, stack', lamexpr')
  Let var valexpr inexpr ->
    eval' (put var valexpr env, stack, inexpr)
  App funexpr valexpr -> case eval' (env, [], valexpr) of
    (env', stack', valexpr') ->
      case eval' (env', valexpr':stack'++stack, funexpr) of
        (env'', [], resexpr) -> (env'', [], resexpr)
        (env'', stack', _) -> (env'', stack, App funexpr valexpr)
  c@(Case sexpr cases) -> case eval' (env, stack, sexpr) of
    (env', stack', Con tag args) -> evalAlt env tag args cases
    _ -> (env, stack, c)
  --where evalstack env stack = evalStack env (stack, [])

-- | Environment that binds variables to values.
type Env = [(Var, Expr)]

put :: Var -> Expr -> Env -> Env
put var expr env = (var, expr):env

-- | Removes the variable var binding from the environment.
taintVar :: Var -> Env -> Env
taintVar var env = case env of
  [] -> []
  (var', expr'):env' -> if var' == var
    then taintVar var env'
    else (var', expr'): taintVar var env'

newenv :: Env
newenv = []

fetch :: Var -> Env -> (Maybe Expr, Env)
fetch var [] = (Nothing, [])
fetch var ((v,e):xs) = if v == var then (Just e, xs) else fetch var xs

-- | Gets the right case alternative.
evalAlt :: Env -> Tag -> [Expr] -> [(Pat, Expr)] -> State
evalAlt env sctag scargs pats = case pats of
  [] -> error ("Constructor tag not found: " ++ sctag)
  ((patexpr, altexpr):pats') -> case eval patexpr of
    Con pattag patargs -> if pattag == sctag
      then case eval' (buildAltEnv scargs patargs env, [], altexpr) of
        (env'', stack'', expr'') -> (env, stack'', expr'')
      else evalAlt env sctag scargs pats'
    _ -> evalAlt env sctag scargs pats'

buildAltEnv :: [Expr] -> [Expr] -> Env -> Env
buildAltEnv scargs patargs env = case (scargs, patargs) of
  ([], []) -> env
  (scarg':scargs', patarg':patargs') -> case patarg' of
    Var var _ -> buildAltEnv scargs' patargs' (put var scarg' env)
    _ -> buildAltEnv scargs' patargs' env
  _ -> error "Incorrect matching case"

-- | The eval function.
eval :: Expr -> Expr
eval expr = selexpr (eval' (newenv, [], expr))
