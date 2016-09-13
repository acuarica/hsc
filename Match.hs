
module Match where

import Expr (Expr(Var, Lam, Let), Var, freeVars)
import Eval (Env, Conf, StackFrame(Update), newConf, emptyEnv, toExpr)
import Simplifier

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

textualMatch :: Conf -> Conf -> Bool
textualMatch (_,Update _:_,_) (_,_,_)          = False
textualMatch (_,_,_)          (_,Update _:_,_) = False
textualMatch (_,_,l)          (_,_,r)          = l == r

-- | Not alpha-equivalence. Free variables equivalence.
-- | Implementation not nice, but nices.
-- | Support only up to 10 arguments!!!
match :: Conf -> Conf -> Bool
match lhs rhs = toExpr lred == toExpr rred
  where
    lexpr = envExpr lhs
    rexpr = envExpr rhs
    lfv   = freeVars lexpr
    rfv   = freeVars rexpr
    llam  = toLambda lfv lexpr
    rlam  = toLambda rfv rexpr
    lred  = freduce  (newConf emptyEnv llam)
    rred  = freduce  (newConf emptyEnv rlam)
    --args  = map (Var . (++) "$a_" . show) [1..10]
