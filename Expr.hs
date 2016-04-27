module Expr where

import Data.Maybe (fromJust)

-- | Represents identifier variable.
type Var = String

-- | Represents constructor name.
type Con = String

-- | The expression type.
data Expr
  = Var  Var
  | Con  Con [Value]
  | Let  Var Expr Expr
  | Lam  Var Expr
  | App  Expr Expr
  | Case Expr [(Pat, Expr)]
  deriving (Eq, Show)

-- | Represents patterns in case expressions.
type Pat = Expr

-- | The Value type represents the result of a computation.
-- | For now, let's use Expr.
type Value = Expr

-- | Environment that binds variables to values.
type Env = [(Var, Value)]

-- | Stack for application calls.
type Stack = [Value]

-- | Pretty prints an expression.
pprint :: Expr -> String
pprint expr = case expr of
  (Var var) -> var
  (Con con args) -> con ++ if null args
    then ""
    else "(" ++ unwords (map pprint args) ++ ")"
  (Lam key expr) -> "(\\" ++ key ++ " -> " ++ pprint expr ++ ")"
  (Let key valexpr inexpr) -> "let " ++ key ++ "=" ++ pprint valexpr ++
    " in " ++ pprint inexpr
  (App aexpr vexpr) ->
    "@(" ++ pprint aexpr ++ ") (" ++ pprint vexpr ++ ")"
  (Case sexpr cs) -> "case " ++ pprint sexpr ++ " of " ++
    foldr (\ (p, e) s -> pprint p ++ " -> " ++ pprint e ++ ";" ++ s) "" cs

-- | Gets the right case alternative.
alt' :: String -> [(Pat, Expr)] -> [(Pat, Expr)] -> Expr
alt' s ps cases = case ps of
  [] -> error ("Con tag not found: " ++ s ++ ": " ++ show cases)
  ((Con s' args, e):ps') -> if s == s' then e else alt' s ps' cases
  ((p, e):ps') -> alt' s ps' cases

evalPat :: (Pat, Expr) -> (Pat, Expr)
evalPat (p, e) = (eval p, e)

-- | Internal eval.
eval' :: Env -> Stack -> Expr -> Value
eval' env stack expr = case expr of
  (Var var) -> case lookup var env of
    Nothing -> Var var
    (Just val) -> eval' env stack val
  (Con con s) -> Con con (s ++ stack)
  lam@(Lam key expr) -> case stack of
      [] -> lam
      (top:rest) -> eval' ((key, top):env) rest expr
  (Let key valexpr inexpr) ->
      eval' ( (key, eval' env stack valexpr) : env) stack inexpr
  (App aexpr vexpr) -> eval' env (eval' env [] vexpr : stack) aexpr
  (Case sexpr cases) -> case eval' env stack sexpr of
    (Con con args) -> eval' env stack (alt' con (map evalPat cases) cases)

-- | The eval function.
eval :: Expr -> Value
eval = eval' [] []
