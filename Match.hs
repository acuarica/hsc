
-- | The Match module defines how two expressions are equivalent.
module Match (match, match', toLambda, envExpr, freduce) where

import Expr (Expr(Var, Lam, Let), Var, freeVars)
import Eval (Env, Conf, StackFrame(Arg, Update),
  newConf, emptyEnv, toExpr, reduce)

-- | freduce reduce also reduces lambda with no arguments.
freduce :: Conf -> Conf
freduce = freduce' 1
  where
   v = Var . (++) "$m_" . show
   freduce' next conf = let (env, stack, expr) = reduce conf in
    case expr of
      Lam var lamexpr ->
        case stack of
          [] -> freduce' (next + 1) (env, [Arg (v next)], Lam var lamexpr)
          _ -> error $ "Stack not empty in freduce" ++ show stack
      _ -> (env, stack, expr)

-- simp :: Conf -> Conf
-- simp conf@(env, stack, expr) = case expr of
--   Var var -> case stack of
--     Alts alts:Alts alts':stack' ->
--       (env, stack', Case (Var var) (map (al alts') alts))
--     _ -> conf
--   _ -> conf
--   where al als (Pat tag vars, altexpr) = (Pat tag vars,
-- Case altexpr als)

-- | Given an Env and an Expr, returns an Expr that includes the Env
-- | by using Let.
envToLet :: Env -> Expr -> Expr
envToLet [] expr = expr
envToLet ((var, valexpr):env) expr = Let var valexpr (envToLet env expr)

-- | Given a Conf, returns the equivalent Expr like toExpr, but also
-- | using the Env.
envExpr :: Conf -> Expr
envExpr conf@(env, _, _) = rebuildEnv env (toExpr conf)
  where rebuildEnv [] expr = expr
        rebuildEnv ((var,valexpr):env) expr =
          Let var valexpr (rebuildEnv env expr)

-- | Given a list of Var, creates a Lam expression where the variables
-- | becomes arguments of the Lam.
toLambda :: [Var] -> Expr -> Expr
toLambda vs expr = foldr Lam expr vs

-- | Not alpha-equivalence. Free variables equivalence.
-- | Implementation not nice, but nices.
match :: Conf -> Conf -> Bool
match lhs rhs =
  toExpr lred == toExpr rred
  where
    lexpr = envExpr $ reduce lhs
    rexpr = envExpr $ reduce rhs
    lfv   = freeVars lexpr
    rfv   = freeVars rexpr
    llam  = toLambda lfv lexpr
    rlam  = toLambda rfv rexpr
    lred  = freduce (newConf emptyEnv llam)
    rred  = freduce (newConf emptyEnv rlam)

-- | What match is doing, but only for one expression.
-- | Not used in match.
match' :: Conf -> Expr
match' lhs = toExpr lred
  where
    lexpr = envExpr $ reduce lhs
    lfv   = freeVars lexpr
    llam  = toLambda lfv lexpr
    lred  = freduce (newConf emptyEnv llam)
