
module Match where

import Expr (Expr(Var, Lam, Let), Var, freeVars)
import Eval (Env, Conf, StackFrame(Update), newConf, emptyEnv, toExpr, reduce)
import Simplifier (freduce)

envToLet :: Env -> Expr -> Expr
envToLet [] expr = expr
envToLet ((var, valexpr):env) expr = Let var valexpr (envToLet env expr)

envExpr :: Conf -> Expr
envExpr conf@(env, stack, expr) = rebuildEnv env (toExpr conf)
  where rebuildEnv [] expr = expr
        rebuildEnv ((var,valexpr):env) expr =
          Let var valexpr (rebuildEnv env expr)

toLambda :: [Var] -> Expr -> Expr
toLambda vs expr = foldr Lam expr vs

type Match = Conf -> Conf -> Bool

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
