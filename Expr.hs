
module Expr where

import Control.Arrow (second)

-- | The expression type.
data Expr
  = Var  Var
  | Con  Tag  [Expr]
  | Let  Var  Expr Expr
  | Lam  Var  Expr
  | App  Expr Expr
  | Case Expr [(Pat, Expr)]
  deriving Eq

-- | Represents identifier variable.
type Var = String

-- | Represents constructor name.
-- | Also called tag to be matched in case expressions.
type Tag = String

-- | Case patterns against tag.
data Pat = Pat Tag [Var] deriving Eq

-- | Variable substitution.
subst :: (Var, Expr) -> Expr -> Expr
subst (var, valexpr) bodyexpr = case bodyexpr of
  Var var' -> if var' == var then valexpr else Var var'
  _ -> apply (subst (var, valexpr)) bodyexpr

substAlts :: [(Var, Expr)] -> Expr -> Expr
substAlts [] bodyexpr = bodyexpr
substAlts (bind:env) bodyexpr = substAlts env (subst bind bodyexpr)

-- | Lookup the alternative according to the constructor tag.
lookupAlt :: Tag -> [(Pat, Expr)] -> (Pat, Expr)
lookupAlt tag ((Pat pattag patvars, expr):alts) = if pattag == tag
  then (Pat pattag patvars, expr)
  else lookupAlt tag alts

-- | Free variables of an expression.
freeVars :: Expr -> [Var]
freeVars expr = case expr of
  Var var -> [var]
  Con _ args -> concatMap freeVars args
  Lam var lamexpr -> del var (freeVars lamexpr)
  Let var valexpr inexpr -> del var (freeVars valexpr ++ freeVars inexpr)
  App funexpr valexpr -> freeVars funexpr ++ freeVars valexpr
  Case scexpr alts -> freeVars scexpr ++
    concatMap (\(Pat p vars, e) -> dels vars (freeVars e)) alts
  where del var xs = [x | x <- xs, x /= var]
        dels vars xs = [x | x <- xs, x `notElem` vars]

-- | Gets all subexpression of an expression.
flatten :: Expr -> [Expr]
flatten expr = expr:case expr of
  Var _ -> []
  Con _ args -> concatMap flatten args
  Lam _ lamexpr -> flatten lamexpr
  Let _ valexpr inexpr -> flatten valexpr ++ flatten inexpr
  App funexpr valexpr -> flatten funexpr ++ flatten valexpr
  Case scexpr alts -> flatten scexpr ++ concatMap (flatten . snd) alts

apply :: (Expr -> Expr) -> Expr -> Expr
apply f expr = case expr of
  Var var -> Var var
  Con tag args -> Con tag (map f args)
  Lam var lamexpr -> Lam var (f lamexpr)
  Let var valexpr inexpr -> Let var (f valexpr) (f inexpr)
  App funexpr valexpr -> App (f funexpr) (f valexpr)
  Case scexpr alts -> Case (f scexpr) (map (second f) alts)

-- | Creates a constructor with the given tag.
con :: Tag -> Expr
con tag = Con tag []
