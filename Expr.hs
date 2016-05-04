
module Expr (
  Var, Tag, Expr(..), Pat,
  untaint, usevar,
  trueTag, falseTag, zeroTag, sucTag, nilTag, consTag,
  true, false, zero, suc, nil, cons
  ) where

import Control.Arrow ((***), second)

-- | The expression type functor.
data ExprF tag var a
  = Var  var Bool
  | Con  tag [a]
  | Let  var a a
  | Lam  var a
  | App  a a
  | Case a [(Pat a, a)]
  deriving (Eq, Show)

-- | Represents patterns in case expressions.
type Pat a = a

instance Functor (ExprF tag var) where
  fmap f expr = case expr of
    Var var tainted -> Var var tainted
    Con tag args -> Con tag (map f args)
    Let var valexpr inexpr -> Let var (f valexpr) (f inexpr)
    Lam var lamexpr -> Lam var (f lamexpr)
    App funexpr valexpr -> App (f funexpr) (f valexpr)
    Case scexpr cases -> Case (f scexpr) (map (f *** f) cases)

-- | Fix point functor.
newtype Fix f = In (f (Fix f))

-- | Set or unsets the tainted flag in variables.
setTaintF :: Bool -> ExprF tag var a
setTaintF tainted expr = case expr of
  Var var _ -> Var var tainted
  expr' -> expr'

--untaint


-- | The expression type.
type Expr = Fix (ExprF Var Tag)

-- | Represents identifier variable.
type Var = String

-- | Represents constructor name.
-- | Also called tag to be matched in case expressions.
type Tag = String

-- | Set or unsets the tainted flag in variables.
setTaint :: Bool -> Expr -> Expr
setTaint tainted expr = case expr of
  Var  var tainted'       -> Var var tainted
  Con  tag args           -> Con tag (map taint args)
  Lam  var lamexpr        -> Lam var (taint lamexpr)
  Let  var valexpr inexpr -> Let var (taint valexpr) (taint inexpr)
  App  funexpr valexpr    -> App (taint funexpr) (taint valexpr)
  Case scexpr cases       -> Case (taint scexpr) (map (second taint) cases)
  where taint = setTaint tainted

-- | Untaints variables in this expression.
untaint :: Expr -> Expr
untaint = setTaint False

-- | Creates a variable untainted.
usevar :: Var -> Expr
usevar var = Var var False

-- | Some common used tags.
trueTag, falseTag, zeroTag, sucTag, nilTag, consTag :: String
trueTag = "True"
falseTag = "False"
zeroTag = "Zero"
sucTag = "Succ"
nilTag = "Nil"
consTag = "Cons"

-- | Some common used expressions.
true, false, zero, suc, nil, cons :: Expr
true = Con trueTag []
false = Con falseTag []
zero = Con zeroTag []
suc = Con sucTag []
nil = Con nilTag []
cons = Con consTag []
