
module Expr (
  Var, Tag, Expr(..), Pat,
  untaint, usevar,
  trueTag, falseTag, zeroTag, sucTag, nilTag, consTag,
  true, false, zero, suc, nil, cons
  ) where

import Control.Arrow (second)

-- | Represents identifier variable.
type Var = String

-- | Represents constructor name.
-- | Also called tag to be matched in case expressions.
type Tag = String

-- | The expression type.
data Expr
  = Var  Var  Bool
  | Con  Tag  [Expr]
  | Let  Var  Expr Expr
  | Lam  Var  Expr
  | App  Expr Expr
  | Case Expr [(Pat, Expr)]
  deriving (Eq, Show)

-- | Represents patterns in case expressions.
type Pat = Expr

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
