
module Eval where

import Debug.Trace
import Control.Arrow (second)

import Expr
import Super

-- | The eval function.
eval :: Expr -> Expr
--eval = selExpr . reduce . newstate
eval = selExpr . nf . newstate

-- | Stack for application calls.
type StackE = [(Env,Expr)]

type Time = Int

type StateE = (Env, StackE, Time, Expr)

selexpr :: StateE -> Expr
selexpr (_, _, _, expr) = expr

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

aform :: Expr -> Expr
aform expr = case aform' (0, [], expr) of
  (fr', env', expr') -> envToLet env' expr'

-- | Internal eval.
eval' :: StateE -> StateE
eval' (env, stack, time, expr) = case expr of
  Var var -> if True
    then (env, stack, nt, Var var)
    else case fetch var env of
      Nothing -> (taintVar var env, stack, nt, Var var)
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
  c@(Case scexpr cases) -> case eval' (env, stack, nt, scexpr) of
    (env', stack', t', Con sctag scargs) -> evalAlt nt env sctag scargs cases
    --(env', stack', t', Var var True) -> (env', stack', t', c)
    (env', stack', t', v@(Var var)) -> (env, stack, nt, Case v (
         map (\(p,e)-> (p, selexpr (eval' (taintVar "cp" env, stack', t', e)) )) cases
         ))
    --(env', stack', t', sc') -> (env', stack', t', c)
    --_ -> (env, stack, nt, c)
  where nt = time + 1
      --(map  (\(p,e)->(p, selexpr (eval' (env, stack, e)) )) cases)
      --)

-- | Removes the variable var binding from the environment.
taintVar :: Var -> Env -> Env
taintVar var env = case env of
  [] -> []
  (var', expr'):env' -> if var' == var
    then taintVar var env'
    else (var', expr'): taintVar var env'

fetch :: Var -> Env -> Maybe Expr
fetch var [] = Nothing
fetch var ((v,e):xs) = if v == var then Just e else fetch var xs

-- | Gets the right case alternative.
evalAlt :: Time -> Env -> Tag -> [Expr] -> [(Pat, Expr)] -> StateE
evalAlt t env sctag scargs pats = case pats of
  [] -> error ("Constructor tag not found: " ++ sctag)
  ((Pat pattag patvars, altexpr):pats') -> --case eval patexpr of
    --Con pattag patargs ->
    if pattag == sctag
      then case eval' (buildAltEnv scargs patvars env, [], t, altexpr) of
        (env'', stack'', t', expr'') -> (env, stack'', t', expr'')
      else evalAlt t env sctag scargs pats'
    --_ -> evalAlt t env sctag scargs pats'

buildAltEnv :: [Expr] -> [Var] -> Env -> Env
buildAltEnv scargs patvars env = case (scargs, patvars) of
  ([], []) -> env
  (scarg':scargs', patvar':patvars') -> --case patvar' of
    --Var var _ ->
    buildAltEnv scargs' patvars' (put patvar' scarg' env)
    --_ -> buildAltEnv scargs' patargs' env
  _ -> error "Incorrect matching case"
