
type Id = String
type Value = Int

data Exp = Var Id |
           Const Int |
           Plus Exp Exp
           deriving Show

eval :: Exp -> Value
eval (Const n) = n
eval (Plus lhs rhs) = (eval lhs) + (eval rhs)

e0 = Const 0
e1 = Const 1
e2 = Const 2
e3 = Plus e0 e1
e4 = Plus e2 e1
e5 = Plus e4 e4
e6 = Plus e5 e4

display e = (show e) ++ " -->> " ++ (show (eval e))

exps = [
  Const 0,
  Const 1,
  Const 2,
  Plus (Const 0) (Const 3),
  Plus (Const 2) (Plus (Const 4) (Const 1)),
  Plus (Plus (Plus (Const 0) (Const 1)) (Const 2)) (Plus (Const 3) (Const 4))
  ]

main = do
  putStrLn "Hola que tal"
  putStrLn (foldr (\s t -> s ++ "\n" ++ t) "" (map display exps))
