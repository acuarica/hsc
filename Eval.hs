
module Eval where

import Expr
import Debug.Trace

-- | Stack for application calls.
type Stack = [(Env,Expr)]

type Time = Int

type State = (Env, Stack, Time, Expr)

selexpr :: State -> Expr
selexpr (_, _, _, expr) = expr

--type Op a =
--type State' = (Env, Stack, Time, [Expr])

--data StackFrame = Val Expr | Thunk Expr
--type Stack' = [StackFrame]

--type Queue = [Expr]
--
-- step :: State' -> State'
-- step (env, stack, time, expr:queue) = case expr of
--   Var var tainted -> case fetch var env of
--     (Just val,env') -> (env, stack, newtime, val:queue)
--   Let var valexpr inexpr ->
--     (put var valexpr env, stack, newtime, inexpr:queue)
--   Lam var lamexpr -> case stack of
--     (env',valexpr):rest ->
--       (put var valexpr env, rest, newtime, lamexpr:queue)
--   App funexpr valexpr ->
--     (env, stack, newtime, Push valexpr:funexpr:queue)
--   where newtime = time + 1

type Fresh = Int

--fapply :: Monad m => m (Expr -> Expr) -> m Expr -> m Expr
--fapply f fexpr 

--isValue
aform :: Fresh -> Expr -> Expr
aform fr expr = case expr of
  App funexpr valexpr ->
    Let (make fr) (apply (aform (fr+1)) valexpr)
      (App (apply (aform (fr+1)) funexpr) (usevar (make fr)))
  expr -> apply (aform (fr+1)) expr
  where make i = "v" ++ "_" ++ show i

subst' :: Var -> Expr -> Expr -> Expr
subst' var expr' expr = case expr of
  v@(Var var' tainted) -> if var' == var then expr' else v
  expr -> apply (subst' var expr') expr

subst :: Var -> Env -> Expr -> Expr
subst var env expr = case fetch var env of
  Nothing -> expr
  Just expr' -> subst' var expr' expr

-- | Internal eval.
eval' :: State -> State
eval' (env, stack, time, expr) = case expr of
  Var var tainted -> if tainted
    then (env, stack, nt, Var var True)
    else case fetch var env of
      Nothing -> (taintVar var env, stack, nt, Var var True)
      Just val -> eval' (env, stack, nt, val)
  Con tag args -> (env, [], nt, Con tag (
    map (selexpr . eval' . (,,,) env [] nt) args ++
    map (\(e,ex)-> selexpr ( eval' (env, [], nt, ex))) stack ))
  Lam var lamexpr -> case stack of
      [] ->
        case eval' (taintVar var env, [], nt, lamexpr) of
          (env', stack', time', lamexpr') ->
            (env, stack', time', Lam var lamexpr')
      (envstack,valexpr):rest ->
        case eval' (put var valexpr env, rest, nt, untaint lamexpr) of
          (env', stack', t, lamexpr') ->
            (env, stack', nt,lamexpr')
  Let var valexpr inexpr ->eval' (put var valexpr env, stack, nt, inexpr)
--    case eval' (put var valexpr env, stack, nt, inexpr) of
  --    (env', stack', nt', resexpr) ->
  App funexpr valexpr -> case eval' (env, [], nt, valexpr) of
    (env', stack', t', valexpr') ->
      case eval' (env', (env,valexpr'):stack'++stack, t', funexpr) of
        (env'', [], t', funexpr') ->
          (env, [], nt, funexpr')
        (env'', stack', t', funexpr') ->
          (env, stack, nt, App funexpr' valexpr')
  c@(Case scexpr cases tainted) -> case eval' (env, stack, nt, scexpr) of
    (env', stack', t', Con sctag scargs) -> evalAlt nt env sctag scargs cases
    --(env', stack', t', Var var True) -> (env', stack', t', c)
    (env', stack', t', v@(Var var True)) -> (env, stack, nt, Case v (
         map (\(p,e)-> (p, selexpr (eval' (taintVar "cp" env, stack', t', e)) )) cases
         ) False)
    --(env', stack', t', sc') -> (env', stack', t', c)
    --_ -> (env, stack, nt, c)
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

fetch :: Var -> Env -> Maybe Expr
fetch var [] = Nothing
fetch var ((v,e):xs) = if v == var then Just e else fetch var xs

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
