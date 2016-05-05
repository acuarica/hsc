
module Expr (
  Var, Tag, Expr(..), Pat,
  untaint, usevar, con,
  true, false, zero, suc, nil, cons
) where

import Control.Arrow (second)

-- | The expression type functor.
data Expr
  = Var  Var  Bool
  | Con  Tag  [Expr]
  | Let  Var  Expr Expr
  | Lam  Var  Expr
  | App  Expr Expr
  | Case Expr [(Pat, Expr)] Bool
  deriving (Eq)

-- | Represents identifier variable.
type Var = String

-- | Represents constructor name.
-- | Also called tag to be matched in case expressions.
type Tag = String

-- | The expression type.
--type Expr = ExprF Var Tag

-- | Represents patterns in case expressions.
type Pat = Expr
--
-- class (Functor f) => Applicative f where
--     pure :: a -> f a
--     (<*>) :: f (a -> b) -> f a -> f b
--(>>=) :: (Monad m) => m a -> (a -> m b) -> m b
--fapply :: (f Expr -> Expr) -> (f Expr -> f Expr) -> f Expr -> f Expr
-- fapply :: (Expr -> t) -> (Expr -> Expr) -> Expr -> t

-- class Context c where
--   --push :: Expr -> c
--   --pull :: c -> Expr
--   sel :: c -> Expr
--
-- newtype S = S (Int, Expr)
--
-- instance Context S where
--   sel (S (n, expr)) = expr

-- fapply :: Context c => (c -> c) -> c -> c
-- fapply e p f cexpr = case sel cexpr of
--   Var  var tainted        -> f (Var var tainted)
--   Lam  var lamexpr        -> f (Lam var (e (p lamexpr)))
--   --Con  tag arg        -> g (Con tag args)

apply :: (Expr -> Expr) -> Expr -> Expr
apply f expr = case expr of
  Var  var tainted          -> Var var tainted
  Con  tag args             -> Con tag (map f args)
  Lam  var lamexpr          -> Lam var (f lamexpr)
  Let  var valexpr inexpr   -> Let var (f valexpr) (f inexpr)
  App  funexpr valexpr      -> App (f funexpr) (f valexpr)
  Case scexpr cases tainted -> Case (f scexpr) (map (second f) cases) tainted

-- | Untaints variables in this expression.
untaint :: Expr -> Expr
untaint expr = case expr of
  Var var _ -> Var var False
  _ -> apply untaint expr

-- | Creates a variable untainted.
usevar :: Var -> Expr
usevar var = Var var False

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
