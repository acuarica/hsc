
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

-- | Stack for Lam calls.
type Stack = [Value]

eval' :: Env -> Stack -> Expr -> Value
eval' env stack expr = case expr of
  (Var var) -> fromJust (lookup var env)
  (Con con s) -> Con con (stack ++ s)
  (Lam key expr) -> eval' ((key, head stack):env) (tail stack) expr
  (Let key valexpr inexpr) -> eval' ( (key, eval' env stack valexpr) : env) stack inexpr
  (App aexpr vexpr) -> eval' env (eval' env stack vexpr :stack) aexpr
  (Case sexpr cs) -> eval' env stack sexpr

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
  --Lam "x" (Var "x"),
  App (Lam "x" (Var "x")) (Con "Z" [])
  -- App (Lam "x" (Var "x")) (Con "Z"),
  -- Let "x" (Con "Z") (Var "x")
  ]

--supercompile :: Expr -> Expr
--supercompile

main :: IO ()
main = print (map (\e -> (e, eval e)) es)
