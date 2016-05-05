
module Eval where

import Expr
import Debug.Trace

-- | Stack for application calls.
type Stack = [Expr]

type Time = Int

type State = (Env, Stack, Time, Expr)

selexpr :: State -> Expr
selexpr (_, _, _, expr) = expr

type State' = (Env, Stack', Time, Expr)

data StackFrame = Val Expr | Thunk Expr
type Stack' = [StackFrame]

--type Queue = [Expr]

step :: State' -> State'
--step (env, Thunk t:stack, time, expr) = step
step (env, stack, time, expr) = case expr of
  Var var tainted -> case fetch var env of
    (Just val,env') -> (env, stack, newtime, val)
  Let var valexpr inexpr ->
    (put var valexpr env, stack, newtime, inexpr)
  Lam var lamexpr -> case stack of
    Val valexpr:rest -> (put var valexpr env, rest, newtime, lamexpr)
  App funexpr valexpr -> (env, Thunk valexpr:stack, newtime, funexpr)
  where newtime = time + 1
  -- case eval' (env, [], nt, valexpr) of
  -- (env', stack', t', valexpr') ->
  --   case eval'  of
  --     (env'', [], t', resexpr) -> (env'', [], nt, resexpr)


-- | Internal eval.
eval' :: State -> State
eval' s@(env, stack, time, expr) = --if time > 200 then s else
  case expr of
  Var var tainted -> if tainted
    then (env, stack, nt, Var var True)
    else case fetch var env of
      (Nothing,_) -> (taintVar var env, stack, nt, Var var True)
      (Just val,env') -> eval' (env, stack, nt, val)
  Con tag args -> (env, [], nt, Con tag (
    map (selexpr . eval' . (,,,) env [] nt) args ++
    map (selexpr . eval' . (,,,) env [] nt) stack ))
  Lam var lamexpr -> case stack of
      [] -> case eval' (taintVar var env, [], nt, lamexpr) of
        (env', stack', time', lamexpr') -> (env, stack', time', Lam var lamexpr')
      valexpr:rest ->
        case eval' (put var valexpr env, rest, nt, untaint lamexpr) of
        (env', stack', t, lamexpr') ->
          (env, stack', nt,lamexpr')
  Let var valexpr inexpr ->
    eval' (put var valexpr env, stack, nt, inexpr)
  App funexpr valexpr ->
    case eval' (env, [], nt, valexpr) of
    (env', stack', t', valexpr') ->
      case eval' (env', valexpr':stack'++stack, t', funexpr) of
      --case eval' (env, valexpr:stack, nt, funexpr) of
        (env'', [], t', resexpr) -> (env'', [], nt, resexpr)
        (env'', stack', t', _) -> (env'', stack, nt, App funexpr valexpr)
  c@(Case scexpr cases tainted) -> case eval' (env, stack, nt, scexpr) of
    (env', stack', t', Con sctag scargs) -> evalAlt nt env sctag scargs cases
    --(env', stack', t', Var var True) -> (env', stack', t', c)
    (env', stack', t', v@(Var var True)) -> (env, stack, nt, Case v (
        map (\(p,e)-> (p, selexpr (eval' (env, stack, nt, e)) )) cases
        ) False)
    --(env', stack', t', sc') -> (env', stack', t', c)
    _ -> (env, stack, nt, c)
  where nt = time + 1
      --(map  (\(p,e)->(p, selexpr (eval' (env, stack, e)) )) cases)
      --)

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
evalAlt :: Time -> Env -> Tag -> [Expr] -> [(Pat, Expr)] -> State
evalAlt t env sctag scargs pats = case pats of
  [] -> error ("Constructor tag not found: " ++ sctag)
  ((patexpr, altexpr):pats') -> case eval patexpr of
    Con pattag patargs -> if pattag == sctag
      then case eval' (buildAltEnv scargs patargs env, [], t, altexpr) of
        (env'', stack'', t', expr'') -> (env, stack'', t', expr'')
      else evalAlt t env sctag scargs pats'
    _ -> evalAlt t env sctag scargs pats'

buildAltEnv :: [Expr] -> [Expr] -> Env -> Env
buildAltEnv scargs patargs env = case (scargs, patargs) of
  ([], []) -> env
  (scarg':scargs', patarg':patargs') -> case patarg' of
    Var var _ -> buildAltEnv scargs' patargs' (put var scarg' env)
    _ -> buildAltEnv scargs' patargs' env
  _ -> error "Incorrect matching case"

-- | The eval function.
eval :: Expr -> Expr
eval = selexpr . eval' . (,,,) newenv [] 0
