
module Expr where

import Control.Arrow (second)

-- | The expression type.
data Expr
  = Var  Var  Bool
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

-- | Case patterns.
data Pat = Pat Tag [String] deriving Eq

-- | Variable substitution.
subst :: Var -> Expr -> Expr -> Expr
subst var valexpr bodyexpr = case bodyexpr of
  Var var' t -> if var' == var then valexpr else Var var' t
  _ -> apply (subst var valexpr) bodyexpr

-- | Lookup the alternative according to the constructor tag.
lookupAlt :: Tag -> [(Pat, Expr)] -> (Pat, Expr)
lookupAlt tag ((Pat pattag patvars, expr):alts) = if pattag == tag
  then (Pat pattag patvars, expr)
  else lookupAlt tag alts

apply :: (Expr -> Expr) -> Expr -> Expr
apply f expr = case expr of
  Var  var tainted -> Var var tainted
  Con  tag args -> Con tag (map f args)
  Lam  var lamexpr -> Lam var (f lamexpr)
  Let  var valexpr inexpr -> Let var (f valexpr) (f inexpr)
  App  funexpr valexpr -> App (f funexpr) (f valexpr)
  Case scexpr alts -> Case (f scexpr) (map (second f) alts)

-- | Untaints variables in this expression.
untaint :: Expr -> Expr
untaint expr = case expr of
  Var var _ -> Var var False
  _ -> apply untaint expr

-- | Creates a variable untainted.
usevar :: Var -> Expr
usevar var = Var var False

-- | Creates a constructor.
con :: Tag -> Expr
con tag = Con tag []

-- | Some common used expressions.
true, false, zero, suc, nil, cons :: Expr
true = con "True"
false = con "False"
zero = con "Zero"
suc = con "Succ"
nil = con "Nil"
cons = con "Cons"
