
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
  deriving Show

-- | Represents patterns in case expressions.
type Pat = Expr

-- | The Value type represents the result of a computation.
-- | For now, let's use Expr.
type Value = Expr

-- | Environment that binds variables to values.
type Env = [(Var, Value)]

-- | Stack for application calls.
type Stack = [Value]

-- | Pretty print an expression.
pprint :: Expr -> String
pprint expr = case expr of
  (Var var) ->
    var
  (Con con args) ->
    con ++ " " ++ foldr (\ arg s -> pprint arg ++ " " ++ s) "" args
  (Lam key expr) ->
    "(\\" ++ key ++ " -> " ++ pprint expr ++ ")"
  (Let key valexpr inexpr) ->
    "let " ++ key ++ "=" ++ pprint valexpr ++ " in " ++ pprint inexpr
  (App aexpr vexpr) ->
    "@(" ++ pprint aexpr ++ ") (" ++ pprint vexpr ++ ")"
  (Case sexpr cs) -> "case " ++ pprint sexpr ++ " of " ++
    foldr (\ (p, e) s -> pprint p ++ " -> " ++ pprint e ++ ";" ++ s) "" cs

eval' :: Env -> Stack -> Expr -> Value
eval' env stack expr = case expr of
  (Var var) -> eval' env stack (fromJust (lookup var env))
  (Con con s) -> Con con (stack ++ s)
  lam@(Lam key expr) -> case stack of
      [] -> lam
      (top:rest) -> eval' ((key, top):env) rest expr
  (Let key valexpr inexpr) ->
      eval' ( (key, eval' env stack valexpr) : env) stack inexpr
  (App aexpr vexpr) -> eval' env (eval' env stack vexpr : stack) aexpr
  (Case sexpr cases) -> case eval' env stack sexpr of
    (Con con args) -> sexpr

eval :: Expr -> Value
eval = eval' [] []

es = [
  --Var "x",
  Con "True" [],
  Con "[]" [],
  Con ":" [],
  Con "Z" [],
  App (Con "S" []) (Con "Z" []),
  App (App (Con ":" []) (Con "Z" [])) (Con "[]" []),
  Lam "x" (Var "x"),
  App (Lam "x" (Var "x")) (Con "Z" []),
  Let "x" (Con "Z" []) (Var "x"),
  Let "x" (Lam "a" (Var "a")) (Let "y" (Con "[]" []) (App (Var "x") (Var "y"))),
  Let "x" (Con "Z" []) (Case (Var "x") [(Con "Z" [], Con "[]" []), (Con "Z" [], Con "[]" [])])
  ]

--supercompile :: Expr -> Expr
--supercompile

doExpr :: Expr -> String
doExpr expr = show expr ++ " | " ++ pprint expr ++ " ~~>> " ++ pprint (eval expr) ++ " | " ++ show (eval expr)

main :: IO ()
main = mapM_ (putStrLn . doExpr) es
