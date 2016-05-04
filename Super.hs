
module Supercompile where


import Expr

supercompile :: Expr -> Expr
supercompile expr = 

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
  Case scexpr cases -> case eval' (env, stack, scexpr) of
    (env', stack', Con sctag scargs) -> evalAlt env sctag scargs cases
    (env', stack', scexpr') -> (env, stack, Case scexpr'
      (map  (\(p,e)->(p, selexpr (eval' (env, stack, e)) )) cases)
      )
