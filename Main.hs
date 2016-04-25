
import Data.Maybe (fromJust)

-- | Represents identifier variable.
type Key = String

-- | Represents constructor name.
type Con = String

-- | The expression type.
data Expr
  = Var  Key
  | Val  Int
  | Con  Con
  | Let  Key Expr Expr
  | Lam  Key Expr
  | App  Expr Expr
  | Case Expr [(Pat, Expr)]
  deriving Show

type Pat = Expr

type Env = [(Key, Int)]

eval' :: Env -> Expr -> Int
eval' env expr = case expr of
  (Var var) -> fromJust (lookup var env)
  (Val val) -> val
  --(Lam key expr) -> eval' ((key, fromJust (lookup key env)):env) expr
  (Let key valexpr inexpr) -> eval' ( (key, eval' env valexpr) : env) inexpr
  (App lam vexpr) -> case lam of
    (Lam key expr) -> eval' ((key, eval' env vexpr):env) expr
  (Case sexpr cs) -> (eval' env sexpr)

eval :: Expr -> Int
eval = eval' []

es = [
  Val 9,
  --Lam "x" (Var "x"),
  App (Lam "x" (Var "x")) (Val 5),
  App (Lam "x" (Var "x")) (Val 5),
  Let "x" (Val 5) (Var "x")
  ]

--supercompile :: Expr -> Expr
--supercompile


main :: IO ()
main = print (map eval es)
