--module NanoParsec where

import ParserCombinator

import Data.Maybe
import Control.Applicative

data Expr
  = Add Expr Expr
  | Mul Expr Expr
  | Sub Expr Expr
  | Div Expr Expr
  | Lit Int
  | Var String
  | Let String Expr Expr
  deriving Show

type Env = [(String, Int)]
eval :: Expr -> Int
eval = eval' []

eval' :: Env -> Expr -> Int
eval' env ex = case ex of
  Add a b -> eval' env a + eval' env b
  Mul a b -> eval' env a * eval' env b
  Sub a b -> eval' env a - eval' env b
  Div a b -> eval' env a `div` eval' env b
  Lit n   -> n
  Var x   -> fromJust (lookup x env)
  Let x a b -> eval' ((x,resa):env) b
    where resa = eval' env a

int :: Parser Expr
int = do
  n <- number
  return (Lit n)

var :: Parser Expr
var = do
  x <- word
  return (Var x)

expr :: Parser Expr
expr = term `chainl1` addop

term :: Parser Expr
term = factor `chainl1` mulop

factor :: Parser Expr
factor =
      int
      <|> letexpr
      <|> var
      <|> parens expr

letexpr :: Parser Expr
letexpr = do
  reserved "let"
  var <- word
  reserved "="
  valexpr <- expr
  reserved "in"
  inexpr <- expr
  return (Let var valexpr inexpr)

infixOp :: String -> (a -> a -> a) -> Parser (a -> a -> a)
infixOp x f = reserved x >> return f

addop :: Parser (Expr -> Expr -> Expr)
addop = infixOp "+" Add <|> infixOp "-" Sub

mulop :: Parser (Expr -> Expr -> Expr)
mulop = infixOp "*" Mul <|> infixOp "/" Div

main :: IO ()
main = do
  run "4"
  run "4      "
  run "      4"
  run "      4      "
  run "   1 + 1   "
  run " 1+10"
  run "  1*10/10"
  run "1*10/20"
  run "1* 10+ 0     "
  run "1 * 10*0"
  run "let lala = 3 in 4"
  run "let lala = 3 in lala"
  run "let x = 4 in x*x+x"
  run "10+   let    var = 20 in var*var+var  "
  run "10+   (let  x = 20 in (x*x+x)+x )  "
  where run s = putStrLn $ str s ++ " > " ++ show (eval (parseWith expr s))
