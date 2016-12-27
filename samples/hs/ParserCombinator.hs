
module ParserCombinator (parseExpr) where

import Data.Char (isDigit, isAlpha, isLower, isUpper)

data Expr
  = Var  Var
  | Con  Tag  [Expr]
  | Lam  Var  Expr
  | App  Expr Expr
  | Let  [Binding] Expr
  | Case Expr [Alt]
  deriving (Eq, Show)

type Var = String
type Tag = String
type Binding = (Var, Expr)
type Alt = (Pat, Expr)
data Pat = Pat Tag [Var] deriving (Eq, Show)

con :: Tag -> Expr
con tag = Con tag []

app :: Expr -> [Expr] -> Expr
app expr args = case args of
  [] -> expr
  arg:args' -> app (App expr arg) args'

true, false, zero, suc, nil, cons :: Expr
true = con "True"
false = con "False"
zero = con "Zero"
suc = con "Succ"
nil = con "Nil"
cons = con "Cons"

nat :: Int -> Expr
nat n = if n > 0 then App suc (nat (n - 1)) else zero

type Chars = Int
data ParserResult a = Error String | Done a Chars String
type Parser a = String -> ParserResult a

pmap :: (a -> b) -> Parser a -> Parser b
pmap f parse s = case parse s of
  Error msg -> Error msg
  Done a chars rest -> Done (f a) chars rest

ppure :: a -> Parser a
ppure a = Done a 0

pseq :: Parser (a -> b) -> Parser a -> Parser b
pseq pf p s = case pf s of
  Error msg -> Error msg
  Done f chars rest -> case p rest of
    Error msg' -> Error msg'
    Done a chars' rest' -> Done (f a) (chars+chars') rest'

pbind :: Parser a -> (a -> Parser b) -> Parser b
pbind p f s = case p s of
  Error msg -> Error msg
  Done a chars rest -> case (f a) rest of
    Error msg' -> Error msg'
    Done b chars' rest' -> Done b (chars+chars') rest'

semi :: Parser a -> Parser b -> Parser b
semi p q = pbind p (\_x -> q)

empty :: Parser a
empty s = Error "Empty parser"

(<|>) :: Parser a -> Parser a -> Parser a
(<|>) p q s = case p s of
  Error msg'p -> case q s of
    Error msg'q -> Error (msg'p ++ " or " ++ msg'q)
    Done a'q c'q r'q -> Done a'q c'q r'q
  Done a'p c'p r'p -> Done a'p c'p r'p

some :: Parser a -> Parser [a]
some v = pseq (pmap (:) v) (many v)

many :: Parser a -> Parser [a]
many v = some v <|> ppure []

item :: Parser Char
item s = case s of
    []     -> Error "Reached EOF"
    (c:cs) -> Done c 1 cs

satisfy :: String -> (Char -> Bool) -> Parser Char
satisfy msg pred = pbind item (\c ->
  if pred c
    then ppure c
    else const (Error ("expecting got " ++ msg ++ [c])))

oneOf :: String -> Parser Char
oneOf s = satisfy s (`elem` s)

char :: Char -> Parser Char
char c = satisfy [c] (c ==)

natural :: Parser Integer
natural = pmap read (some (satisfy "isDigit" isDigit))

string :: String -> Parser String
string [] = ppure []
string (c:cs) = semi (char c) $ semi (string cs) $ ppure (c:cs)

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
  spaces
  return (read (s ++ cs))

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

sat :: String -> Parser String -> (String -> Bool) -> Parser String
sat msg p pred = (>>=) p (\s -> if pred s
    then return s
    else Parser (const (Error (printf "expecting %s got %s" msg s))))

parens :: Parser a -> Parser a
parens m = do { reserved "("; n <- m; reserved ")"; return n }

braces :: Parser a -> Parser a
braces m = do { reserved "{"; n <- m; reserved "}"; return n }

brackets :: Parser a -> Parser a
brackets m = do { reserved "["; n <- m; reserved "]"; return n }

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
  reserved "let"
  binds <- bindsp
  --var <- varnamep
  --reserved "="
  --valexpr <- exprp
  reserved "in"
  inexpr <- exprp
  return (Let binds inexpr)

bindsp :: Parser [Binding]
bindsp = do
        var <- varnamep
        reserved "="
        valexpr <- exprp
        (do
          reserved ";"
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
  reserved "->"
  valexpr <- exprp
  return (Lam var valexpr)

casep :: Parser Expr
casep = do
  reserved "case"
  scexpr <- exprp
  reserved "of"
  alts <- some altp
  return (Case scexpr alts)

altp :: Parser (Pat, Expr)
altp = do
  tag <- upperword
  vars <- many varnamep
  reserved "->"
  res <- exprp
  reserved ";"
  return (Pat tag vars, res)

listp :: Parser Expr
listp = (do
      item <- exprp
      (do
        reserved ","
        rest <- listp
        return (app cons [item, rest])) <|>
        return (app cons [item, nil])
    ) <|> return nil

varnamep :: Parser Var
varnamep = sat (show keywords) varid (not . (`elem` keywords))
  where keywords = ["let", "in", "case", "of"]

varid :: Parser Var
varid = do
  c  <- loweralpha <|> dollar <|> underscore
  cs <- many (alpha <|> digit <|> underscore <|> quote)
  spaces
  return (c:cs)

parseWith :: Show a => Parser a -> String -> a
parseWith p s =
  case parse (do { spaces; p }) s of
    Done a _ [] -> a
    Done a chars rest -> error $
      "Parser didn't consume entire stream: <<" ++ rest ++ ">> " ++
      " in <<" ++ s ++ ">> at " ++ show chars ++
                   " with " ++ show a
    Error msg  -> error $ printf "Parser error: %s in ``%s''" msg s

parseExpr :: String -> Expr
parseExpr = parseWith exprp
