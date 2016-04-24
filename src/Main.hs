
import Data.Maybe (fromJust)

type Key = String

data Expr
  = Var  Key
  | Val  Int
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
  (App lam vexpr) -> case lam of
    (Lam key expr) -> eval' ((key, eval' env vexpr):env) expr
  (Case sc cs) -> 0

eval :: Expr -> Int
eval = eval' []

es = [
  Val 9,
  --Lam "x" (Var "x"),
  App (Lam "x" (Var "x")) (Val 5),
  App (Lam "x" (Var "x")) (Val 5)
  ]

--supercompile :: Expr -> Expr
--supercompile

main :: IO ()
main = print (map eval es)
