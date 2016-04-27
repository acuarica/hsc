module Expr where

import Data.Maybe (fromJust)

-- | Represents identifier variable.
type Var = String

-- | Represents constructor name.
-- | Also called tag to be matched in case expressions.
type Tag = String

-- | The expression type.
data Expr
  = Var  Var
  | Con  Tag [Value]
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

-- | Internal eval.
eval' :: Env -> Stack -> Expr -> Value
eval' env stack expr = case expr of
  (Var var) -> case lookup var env of
    Nothing -> Var var
    (Just val) -> eval' env stack val
  (Con tag s) -> Con tag (map (eval' env []) s ++ stack)
  (Lam var expr') -> case stack of
      --[] -> Lam var expr'
      (top:rest) -> eval' ((var, top):env) rest expr'
  (Let var valexpr inexpr) -> eval' ((var, valexpr):env) stack inexpr
  (App funexpr valexpr) -> eval' env (eval' env [] valexpr:stack) funexpr
  (Case sexpr cases) -> case eval' env stack sexpr of
    (Con tag args) -> evalAlt env tag args cases

-- | Gets the right case alternative.
evalAlt :: Env -> Tag -> [Value] -> [(Pat, Expr)] -> Expr
evalAlt env sctag scargs pats = case pats of
  [] -> error ("Constructor tag not found: " ++ sctag)
  ((patexpr, altexpr):pats') -> case eval patexpr of
    (Con pattag patargs) -> if pattag == sctag
      then eval' (buildAltEnv scargs patargs env) [] altexpr
      else evalAlt env sctag scargs pats'
    _ -> evalAlt env sctag scargs pats'

buildAltEnv :: [Value] -> [Value] -> Env -> Env
buildAltEnv scargs patargs env = case (scargs, patargs) of
  ([], []) -> env
  (scarg':scargs', patarg':patargs') -> case patarg' of
    Var var -> buildAltEnv scargs' patargs' ((var, scarg'):env)
    _ -> buildAltEnv scargs' patargs' env
  _ -> error "Incorrect matching case"

-- | The eval function.
eval :: Expr -> Value
eval = eval' [] []
