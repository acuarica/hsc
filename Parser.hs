{-|
  Parser for Expr.
-}
module Parser (parseExpr) where

import Expr (Expr(Var, Lam, App, Let, Case), Var, Binding, Pat(Pat),
  con, app, nil, cons, nat)
import Data.Char (isDigit, isAlpha, isLower, isUpper)
import Control.Applicative (Alternative, empty, (<|>), some, many)
import Text.Printf (printf)

-- | Represents how many characters are consumed within the parsed string.
type Chars = Int

-- | The result of parsing. It can be an Error or Done.
-- | Done contains the value, line, column and the tail to be parsed.
data ParserResult a = Error String | Done a Chars String

-- | The Parser type. A parser takes a String, process the appropiate
-- | parser, and returns a ParserResult.
newtype Parser a = Parser { parse :: String -> ParserResult a }

instance Functor Parser where
  fmap f (Parser p) = Parser (\s ->
    case p s of
      Error msg -> Error msg
      Done a chars rest -> Done (f a) chars rest
    )

instance Applicative Parser where
  pure a = Parser (Done a 0)

  (Parser pf) <*> (Parser p) = Parser (\s ->
    case pf s of
      Error msg -> Error msg
      Done f chars rest -> case p rest of
        Error msg' -> Error msg'
        Done a chars' rest' -> Done (f a) (chars+chars') rest'
    )

instance Monad Parser where
  return = pure

  (>>=) (Parser p) f = Parser (\s ->
    case p s of
      Error msg -> Error msg
      Done a chars rest -> case parse (f a) rest of
        Error msg' -> Error msg'
        Done b chars' rest' -> Done b (chars+chars') rest'
    )

instance Alternative Parser where
  empty = Parser (const (Error "Empty parser"))

  (<|>) (Parser p) (Parser q) = Parser (\s ->
      case p s of
        Error msg'p -> case q s of
          Error msg'q -> Error (msg'p ++ " or " ++ msg'q)
          done -> done
        done -> done
    )

-- | Parses a char.
item :: Parser Char
item = Parser (\s -> case s of
    []     -> Error "Reached EOF"
    (c:cs) -> Done c 1 cs)

satisfy :: String -> (Char -> Bool) -> Parser Char
satisfy msg predicate = (>>=) item (\c ->
  if predicate c
    then return c
    else Parser (const (Error (printf "expecting %s got %c" msg c))))

oneOf :: String -> Parser Char
oneOf s = satisfy s (`elem` s)

char :: Char -> Parser Char
char c = satisfy [c] (c ==)

string :: String -> Parser String
string [] = return []
string (c:cs) = char c >> string cs >> return (c:cs)

token :: Parser a -> Parser a
token p = do { a <- p; spaces >> return a}

reserved :: String -> Parser String
reserved s = token (string s)

spaces :: Parser String
spaces = many $ oneOf " \n\r"

digit :: Parser Char
digit = satisfy "isdigit" isDigit

alpha :: Parser Char
alpha = satisfy "isalpha" isAlpha

loweralpha :: Parser Char
loweralpha = satisfy "loweralpha" (\c -> isAlpha c && isLower c)

upperalpha :: Parser Char
upperalpha = satisfy "upperalpha" (\c -> isAlpha c && isUpper c)

dollar :: Parser Char
dollar = satisfy "dollar sign" (== '$')

underscore :: Parser Char
underscore = satisfy "underscore" (== '_')

quote :: Parser Char
quote = satisfy "quote" (== '\'')

number :: Parser Int
number = do
  s <- string "-" <|> return []
  cs <- some digit
  _ <- spaces
  return (read (s ++ cs))

upperword :: Parser String
upperword = do
  c  <- upperalpha
  cs <- many alpha
  _ <- spaces
  return (c:cs)

sat :: String -> Parser String -> (String -> Bool) -> Parser String
sat msg p predicate = (>>=) p (\s -> if predicate s
    then return s
    else Parser (const (Error (printf "expecting %s got %s" msg s))))

enclose :: String -> String -> Parser a -> Parser a
enclose o c m = do { _ <- reserved o; n <- m; _ <- reserved c; return n }

parens :: Parser a -> Parser a
parens = enclose "(" ")"

braces :: Parser a -> Parser a
braces = enclose "{" "}"

brackets :: Parser a -> Parser a
brackets = enclose "[" "]"

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainl1` op = do {a <- p; rest a}
  where rest a = (do {f <- op; b <- p; rest (f a b)}) <|> return a

chainr1 :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainr1` op = do {a <- p; rest a}
  where rest a = (do {f <- op; b <- p; b' <- rest b; return (f a b')})
          <|> return a

exprp :: Parser Expr
exprp = (termp `chainr1` conslistp) `chainl1` return App

conslistp :: Parser (Expr -> Expr -> Expr)
conslistp = reserved ":" >> return (App . App cons)

termp :: Parser Expr
termp = litintp
    <|> letp
    <|> casep
    <|> varp
    <|> conp
    <|> braces lamp
    <|> parens exprp
    <|> brackets listp

litintp :: Parser Expr
litintp = do { n <- number; return (nat n) }

letp :: Parser Expr
letp = do
  _ <- reserved "let"
  binds <- bindsp
  --var <- varnamep
  --reserved "="
  --valexpr <- exprp
  _ <- reserved "in"
  inexpr <- exprp
  return (Let binds inexpr)

bindsp :: Parser [Binding]
bindsp = do
        var <- varnamep
        _ <- reserved "="
        valexpr <- exprp
        (do
          _ <- reserved ";"
          binds <- bindsp
          return ((var, valexpr):binds) ) <|>
          return [(var, valexpr)]

varp :: Parser Expr
varp = do
  var <- varnamep
  return (Var var)

conp :: Parser Expr
conp = do { tag <- upperword; return (con tag) }

lamp :: Parser Expr
lamp = do
  var <- varnamep
  _ <- reserved "->"
  valexpr <- exprp
  return (Lam var valexpr)

casep :: Parser Expr
casep = do
  _ <- reserved "case"
  scexpr <- exprp
  _ <- reserved "of"
  alts <- some altp
  return (Case scexpr alts)

altp :: Parser (Pat, Expr)
altp = do
  tag <- upperword
  vars <- many varnamep
  _ <- reserved "->"
  res <- exprp
  _ <- reserved ";"
  return (Pat tag vars, res)

listp :: Parser Expr
listp = (do
      listelem <- exprp
      (do
        _ <- reserved ","
        rest <- listp
        return (app cons [listelem, rest])) <|>
        return (app cons [listelem, nil])
    ) <|> return nil

varnamep :: Parser Var
varnamep = sat (show keywords) varid (not . (`elem` keywords))
  where keywords = ["let", "in", "case", "of"]

varid :: Parser Var
varid = do
  c  <- loweralpha <|> dollar <|> underscore
  cs <- many (alpha <|> digit <|> underscore <|> quote)
  _ <- spaces
  return (c:cs)

parseWith :: Show a => Parser a -> String -> a
parseWith p s =
  case parse (spaces >> p) s of
    Done a _ [] -> a
    Done a chars rest -> error $
      "Parser didn't consume entire stream: <<" ++ rest ++ ">> " ++
      " in <<" ++ s ++ ">> at " ++ show chars ++
                   " with " ++ show a
    Error msg  -> error $ printf "Parser error: %s in ``%s''" msg s

{-|
  Given a textual representation of Expr, returns the parsed Expr.
-}
parseExpr :: String -> Expr
parseExpr = parseWith exprp
