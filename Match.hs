{-|
  The Match module defines how two expressions are equivalent.
-}
module Match (
  match, match', toLambda, envExpr, freduce, (|~~|), (|><|), (<|)
) where

import Data.Maybe (fromJust)
import Data.List (delete)

import Expr (
  Expr(Var, Con, Lam, Let, App, Case), Var, Subst, let1, freeVars)
import Eval (Env, Conf, StackFrame(Arg, Update),
  newConf, emptyEnv, toExpr, reduce)

{-|
  freduce reduce also reduces lambda with no arguments.
-}
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

{-|
  Given an Env and an Expr, returns an Expr that includes the Env
  by using Let.
-}
envToLet :: Env -> Expr -> Expr
envToLet [] expr = expr
envToLet ((var, valexpr):env) expr = let1 var valexpr (envToLet env expr)

{-|
  Given a Conf, returns the equivalent Expr like toExpr,
  but also using the Env.
-}
envExpr :: Conf -> Expr
envExpr conf@(env, _, _) = rebuildEnv env (toExpr conf)
  where rebuildEnv [] expr = expr
        rebuildEnv ((var,valexpr):env) expr =
          Let [(var, valexpr)] (rebuildEnv env expr)

{-|
  Given a list of Var, creates a Lam expression where the variables
  becomes arguments of the Lam.
-}
toLambda :: [Var] -> Expr -> Expr
toLambda vs expr = foldr Lam expr vs

{-|
  Not alpha-equivalence. Free variables equivalence.
  Implementation not nice, but nices.
-}
match :: Conf -> Conf -> Bool
match lhs rhs = match' lhs == match' rhs

{-|
  What match is doing, but only for one expression.
  Not used in match.
-}
match' :: Conf -> Expr
match' = toExpr . freduce . newConf emptyEnv . llam . envExpr . reduce
  where llam expr = toLambda (freeVars expr) expr

merge :: [Subst] -> [Subst] -> [Subst]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) = merge' x y ++ merge xs (y:ys)

merge' (v1, e1) (v2, e2) = if v1 == v2
  then fromJust $ e1 |~~| e2
  else [(v1, e1)]

uni :: [Maybe [Subst]] -> Maybe [Subst]
uni [] = Just []
uni [x] = x
uni (x:y:xs) = merge <$> x <*> uni (y:xs)

{-|
  Unification of two expressions.
  Examples:
  >  Cons x xs |~~|
  >    Cons 2 Nil
  >    with subst := [x |-> 2, xs -> Nil]
  >  Branch 2 t t |~~| Branch v x y
  >    with subst [val |-> 2, t |-> x, x|->y]
-}
(|~~|) :: Expr -> Expr -> Maybe [Subst]
(|~~|) (Var v) (Var w) = Just $ if v == w then [] else [(v, Var w)]
(|~~|) (Var v) e = if v `elem` freeVars e then Nothing else Just [(v, e)]
(|~~|) e (Var v) = if v `elem` freeVars e then Nothing else Just [(v, e)]
(|~~|) (Con tag1 args1) (Con tag2 args2) =
  if tag1 == tag2 && length args1 == length args2
    then uni (zipWith (|~~|) args1 args2)
    else Nothing
(|~~|) (Lam v1 e1) (Lam v2 e2) =
  if v1 == v2
    then e1 |~~| e2
    else case e1 |~~| e2 of
      Nothing -> Nothing
      Just s -> if (v1, Var v2) `elem` s
        then Just $ delete (v1, Var v2) s
        else Nothing
--(|~~|) (Let v1 e1 b1) (Let v2 e2 b2) =
  -- merge <$> e1 |~~| e2 <*> b1 |~~| b2
(|~~|) (App f1 v1) (App f2 v2) = merge <$> f1 |~~| f2 <*> v1 |~~| v2

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

snd'trd :: (a, b, c) -> (b, c)
snd'trd (_, y, z) = (y, z)

{-|
  Generalization algorithm.
  plus n m |><| plus n (Succ m) =
    plus n $0 << $0->m | $0 -> Succ m
-}
(|><|) :: Expr -> Expr -> (Expr, [(Var, Expr)], [(Var, Expr)])
(|><|) (Var v) (Var w) = if v == w
  then (Var v, [], [])
  else let newvar = "$2" in (Var newvar, [(newvar, Var v)], [(newvar, Var w)])
(|><|) e@(Con tag args) e'@(Con tag' args') =
  if tag == tag' && length args == length args'
  then let a = (zipWith (\x y-> fst3 $ (|><|) x y) args args') in
    (Con tag a, [], [])
  else error "Nogen@Con"
-- and (zipWith (<|) args1 args2)
(|><|) (App f1 v1) (App f2 v2) =
  let (fg, fs1, fs2) = f1 |><| f2
      (vg, vs1, vs2) = v1 |><| v2
  in  (App fg vg, fs1 ++ vs1, fs2 ++ vs2)
(|><|) e1 (App _f1 e2) =
  let (eg, s1, s2) = e1 |><| e2
      newvar = "$0" in
  (Var newvar, [(newvar, e1)], [(newvar, App _f1 e2)])
(|><|) (App _f1 e2) e1 =
  let (eg, s1, s2) = e1 |><| e2
      newvar = "$0" in
  (Var newvar, [(newvar, App _f1 e2)], [(newvar, e1)] )
(|><|) expr@(Case sc alts) expr'@(Case sc' alts') =
  if length alts == length alts' && and (zipWith (\(p,_)(q,_)->p==q) alts alts')
  then let as = zipWith ((\(p,i)(_,j)-> (p, fst3 $ i |><| j) )) alts alts'
           (e, s, t) = sc |><| sc'
           (s', t') = unzip $ zipWith ((\(p,i)(_,j)-> snd'trd$i |><| j)) alts alts'
       in  (Case e as, s ++ concat s', t ++ concat t')
  else -- trace ("Nogen@Case: " ++ show expr ++ "\n" ++ show expr') $
    let newvar = "$3"
     in (Var newvar, [(newvar, expr)], [(newvar, expr')])
(|><|) e e' = error $ show e ++ " |><| " ++ show e'

{-|
  Homeomorphic Embedding relation.
  Given two expressions e1, e2, we say `e1 <| e2` (e1 is embedded in e2).
  plus n m <| plus n (Succ m)
 -}
(<|) :: Expr -> Expr -> Bool
-- Coupling
(<|) (Var v) (Var v') = True--v == v' --True
(<|) (Con tag1 args1) (Con tag2 args2) =
  tag1 == tag2 &&
  length args1 == length args2 &&
  and (zipWith (<|) args1 args2)
(<|) (Lam _ e1) (Lam _ e2) = e1 <| e2
(<|) (App f1 v1) (App f2 v2) = f1 <| f2 && v1 <| v2

-- Diving
(<|) e1 (App _f1 e2) = e1 <| e2

(<|) e e'@(Case sc alts) =
  -- Diving in case
  e <| sc ||
  -- TODO: Should only be HE in the sc or also the alts?
  -- any (\(p', e')-> e <| e') alts ||
  -- Coupling for case
  coupCase e e'
  where
    -- TODO: Patterns should be a renaming, not syntactially equal.
    coupCase (Case sc alts) (Case sc' alts') =
      sc <| sc' &&
      length alts == length alts' &&
      and (zipWith (\(p, e) (p', e') -> p == p' && e <| e') alts alts')
    coupCase _ _ = False

--emb (App _ _) (App _ _) =
(<|) _ _ = False
