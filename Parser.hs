
module Parser where

import Expr
import Data.Char
import Control.Monad
import Control.Applicative
import Text.Printf

-- | Represents how many characters are consumed within the parsed string.
type Chars = Int

-- | The result of parsing. It can be an Error or Done.
-- | Done contains the value, line, column and the tail to be parsed.
data ParserResult a = Error String | Done a Chars String

-- | The Parser type. A parser takes a String, process the appropiate
-- | parser, and returns a ParserResult.
newtype Parser a = Parser { parse :: String -> ParserResult a }

instance Functor Parser where
  fmap f (Parser parse) = Parser (\s ->
    case parse s of
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
        Error msg -> q s
        res -> res
    )

-- | Parses a char
item :: Parser Char
item = Parser (\s -> case s of
    []     -> Error "Reached EOF"
    (c:cs) -> Done c 1 cs)

satisfy :: String -> (Char -> Bool) -> Parser Char
satisfy msg pred = (>>=) item (\c ->
  if pred c
    then return c
    else Parser (const (Error (printf "expecting %s got %c" msg c))))

oneOf :: String -> Parser Char
oneOf s = satisfy s (`elem` s)

char :: Char -> Parser Char
char c = satisfy [c] (c ==)

natural :: Parser Integer
natural = fmap read (some (satisfy "isDigit" isDigit))

string :: String -> Parser String
string [] = return []
string (c:cs) = do { char c; string cs; return (c:cs)}

token :: Parser a -> Parser a
token p = do { a <- p; spaces ; return a}

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

number :: Parser Int
number = do
  s <- string "-" <|> return []
  cs <- some digit
  spaces
  return (read (s ++ cs))

word :: Parser String
word = do
  cs <- some alpha
  spaces
  return cs

lowerword :: Parser String
lowerword = do
  c  <- loweralpha
  cs <- many alpha
  spaces
  return (c:cs)

upperword :: Parser String
upperword = do
  c  <- upperalpha
  cs <- many alpha
  spaces
  return (c:cs)

dollarword :: Parser String
dollarword = do
  d <- char '$'
  w <- lowerword
  return (d:w)

sat :: Parser String -> (String -> Bool) -> Parser String
sat p pred = (>>=) p (\s ->
  if pred s
    then return s
    else Parser (const (Error (printf "expecting got %s" s))))

parens :: Parser a -> Parser a
parens m = do { reserved "("; n <- m; reserved ")"; return n }

braces :: Parser a -> Parser a
braces m = do { reserved "{"; n <- m; reserved "}"; return n }

brackets :: Parser a -> Parser a
brackets m = do { reserved "["; n <- m; reserved "]"; return n }

str s = "``" ++ s ++ "''"

parseWith :: Show a => Parser a -> String -> a
parseWith p s =
  case parse (do { spaces; p }) s of
    Done a _ [] -> a
    Done a chars rest -> error $ "Parser didn't consume entire stream: "++
      str rest ++ " in " ++ str s ++ " at " ++ show chars ++
                   " with " ++ show a
    Error msg  -> error $ printf "Parser error: %s in ``%s''" msg s

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainl1` op = do {a <- p; rest a}
  where rest a = (do {f <- op; b <- p; rest (f a b)}) <|> return a

exprp :: Parser Expr
exprp = termp `chainl1` return App

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
litintp = do { n <- number; return (f n) }
  where f n = if n == 0 then zero else App suc (f (n-1))

listp :: Parser Expr
listp = (do
      item <- exprp
      (do
        reserved ","
        rest <- listp
        return (App (App cons item) rest)) <|>
        return (App (App cons item) nil)
    ) <|> return nil

varp :: Parser Expr
varp = do { v <- dollarword; return (newvar v) }

conp :: Parser Expr
conp = do { x <- upperword; return (Con x []) }

lamp :: Parser Expr
lamp = do
  reserved "\\"
  var <- dollarword
  reserved "->"
  valexpr <- exprp
  return (Lam var valexpr)

letp :: Parser Expr
letp = do
  reserved "let"
  var <- dollarword
  reserved "="
  valexpr <- exprp
  reserved "in"
  inexpr <- exprp
  return (Let var valexpr inexpr)

casep :: Parser Expr
casep = do
  reserved "case"
  sc <- exprp
  reserved "of"
  reserved "{"
  alts <- some altp
  reserved "}"
  return (Case sc alts)

altp :: Parser (Pat, Expr)
altp = do
    alt <- exprp
    reserved "->"
    res <- exprp
    reserved ";"
    return (alt, res)

parseExpr :: String -> Expr
parseExpr = parseWith exprp

newvar :: Var -> Expr
newvar var = Var var False

true, false, zero, suc, nil, cons :: Expr
true = Con "True" []
false = Con "False" []
zero = Con "Zero" []
suc = Con "Succ" []
nil = Con "Nil" []
cons = Con "Cons" []
