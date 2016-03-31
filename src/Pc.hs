--module NanoParsec where

import ParserCombinator

import Data.Maybe
import Control.Applicative

type Id = String

data Expr a
  = Add (Expr a) (Expr a)
  | Mul (Expr a) (Expr a)
  | Sub (Expr a) (Expr a)
  | Div (Expr a) (Expr a)
  | Lit Int
  | Var String
  | Let String (Expr a) (Expr a)
  | Lam String (Expr a)
  | App (Expr a) (Expr a)
  deriving Show

instance Functor Expr where
  fmap f (Add l r) = Add (fmap f l) (fmap f r)
  fmap f (Mul l r) = Mul (fmap f l) (fmap f r)
  fmap f (Sub l r) = Sub (fmap f l) (fmap f r)
  fmap f (Div l r) = Div (fmap f l) (fmap f r)
  fmap f (Lit n) = Lit n
  fmap f (Var x) = Var x
  fmap f (Let x a b) = Let x (fmap f a) (fmap f b)

type Env = [(String, Int)]
eval :: Expr a -> Int
eval = eval' []

eval' :: Env -> Expr a -> Int
eval' env ex = case ex of
  Add a b -> eval' env a + eval' env b
  Mul a b -> eval' env a * eval' env b
  Sub a b -> eval' env a - eval' env b
  Div a b -> eval' env a `div` eval' env b
  Lit n   -> n
  Var x   -> fromJust (lookup x env)
  Let x a b -> eval' ((x, eval' env a):env) b
  Lam x a -> 10

infixOp :: String -> (a -> a -> a) -> Parser (a -> a -> a)
infixOp op cons = reserved op >> return cons

int = do { n <- number; return (Lit n) }

var = do { x <- word; return (Var x) }

expr :: Parser (Expr a)
expr = do
  a <- term `chainl1` addop
  --string "eol"
  return a

term = factor `chainl1` mulop

factor = int
     <|> letexpr
     <|> braces lamexpr
     <|> var
     <|> parens expr

lamexpr = do
  reserved "\\"
  var <- word
  reserved "->"
  valexpr <- expr
  return (Lam var valexpr)

letexpr = do
  reserved "let"
  var <- word
  reserved "="
  valexpr <- expr
  reserved "in"
  inexpr <- expr
  return (Let var valexpr inexpr)

addop = infixOp "+" Add <|> infixOp "-" Sub

mulop = infixOp "*" Mul <|> infixOp "/" Div


data Regex = Char
           | Star Regex

--regexParser :: Regex
main :: IO ()
main = do
  --run "-"
  --run "12+%34eol"
  run "4      "
  run "      4"
  run "      4      "
  run "   1 + 1   "
  run " 1+10"
  run "  1*10/10"
  run "1*10/20"
  run "1* 10+ 0     "
  run "1 * 10*0"
  run "{\\ x  -> x+1}"
  run "1+1+1+1+1+2 + []"
  --run "-1 asdfasdf 1 ++ 2"
  --run "-"
  run "let lala = 3 in 4"
  run "let lala = 3 in lala"
  run "let x = 4 in x*x+x"
  run "10+   let    var = 20 in var*var+var  "
  run "10+   (let  x = 20 in (x*x+x)+x )  "
  where run s = putStrLn $ str s ++ " > " ++ show (eval (parseWith expr s))
