
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

type Chars = Int
data ParserResult a = Error String | Done a Chars String
type Parser a = String -> ParserResult a

c' :: (b -> c) -> (a -> b) -> a -> c
c' f g x = f (g x)

not' :: Bool -> Bool
not' b = case b of False -> True; True -> False

const' :: a -> b -> a
const' x _y = x

app' :: [a] -> [a] -> [a]
app' xs ys = case xs of
  [] -> ys
  (x:xs') -> x : app' xs' ys

isElem :: Eq a => a -> [a] -> Bool
isElem x ys = case ys of [] -> False; (y:ys') -> x == y || isElem x ys'

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
  Done a chars rest -> case f a rest of
    Error msg' -> Error msg'
    Done b chars' rest' -> Done b (chars+chars') rest'

semi :: Parser a -> Parser b -> Parser b
semi p q = pbind p (\_x -> q)

empty :: Parser a
empty s = Error "Empty parser"

por :: Parser a -> Parser a -> Parser a
por p q s = case p s of
  Error msg'p -> case q s of
    Error msg'q -> Error (msg'p `app'` " or " `app'` msg'q)
    Done a'q c'q r'q -> Done a'q c'q r'q
  Done a'p c'p r'p -> Done a'p c'p r'p

some :: Parser a -> Parser [a]
some v = pseq (pmap (:) v) (many v)

many :: Parser a -> Parser [a]
many v = some v `por` ppure []

item :: Parser Char
item s = case s of
    []     -> Error "Reached EOF"
    (c:cs) -> Done c 1 cs

satisfy :: String -> (Char -> Bool) -> Parser Char
satisfy msg pred = pbind item (\c ->
  if pred c
    then ppure c
    else const' (Error ("expecting got " `app'` msg `app'` [c])))

oneOf :: String -> Parser Char
oneOf s = satisfy s (\c -> c `isElem` s)

char :: Char -> Parser Char
char c = satisfy [c] (\c' -> c == c')

natural :: Parser Integer
natural = pmap read (some (satisfy "isDigit" isDigit))

string :: String -> Parser String
string xs = case xs of
  [] -> ppure []
  (c:cs) -> semi (char c) (semi (string cs) (ppure (c:cs)) )

token :: Parser a -> Parser a
token p =  p `pbind` (\a-> spaces `semi` ppure a)

reserved :: String -> Parser String
reserved s = token (string s)

spaces :: Parser String
spaces = many (oneOf " \n\r")

digit :: Parser Char
digit = satisfy "isdigit" isDigit

alpha :: Parser Char
alpha = satisfy "isalpha" isAlpha

loweralpha :: Parser Char
loweralpha = satisfy "loweralpha" (\c -> isAlpha c && isLower c)

upperalpha :: Parser Char
upperalpha = satisfy "upperalpha" (\c -> isAlpha c && isUpper c)

dollar :: Parser Char
dollar = satisfy "dollar sign" (\c->c== '$')

underscore :: Parser Char
underscore = satisfy "underscore" (\c->c== '_')

quote :: Parser Char
quote = satisfy "quote" (\c->c== '\'')

number :: Parser Int
number =
  string "-" `por` ppure [] `pbind` \s->
  some digit `pbind` \cs->
  spaces `semi`
  ppure (read (s `app'` cs))

lowerword :: Parser String
lowerword =
  loweralpha `pbind` \c->
  many alpha `pbind` \cs ->
  spaces `semi`
  ppure (c:cs)

upperword :: Parser String
upperword =
  upperalpha `pbind` \c->
  many alpha `pbind` \cs->
  spaces `semi`
  ppure (c:cs)

sat :: String -> Parser String -> (String -> Bool) -> Parser String
sat msg p pred = p `pbind` \s -> if pred s
    then ppure s
    else const' (Error "sat")

paired :: String -> String -> Parser a -> Parser a
paired o c m = reserved o `semi`  m `pbind` (\n-> reserved c `semi` ppure n )

parens :: Parser a -> Parser a
parens = paired "(" ")"

braces :: Parser a -> Parser a
braces = paired "{" "}"

brackets :: Parser a -> Parser a
brackets = paired "[" "]"

restl p op a = (op `pbind` (\f-> p `pbind` (\b-> restl p op (f a b) ))) `por` ppure a
chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op = p `pbind` (\a->restl p op a)

restr p op a =(op`pbind` (\f-> p `pbind` (\b-> restr p op b `pbind` (\b'-> ppure (f a b')))))
          `por` ppure a
chainr1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainr1 p op = p `pbind` (\a-> restr p op  a)

exprp :: Parser Expr
exprp = (termp `chainr1` conslistp) `chainl1` ppure App

conslistp :: Parser (Expr -> Expr -> Expr)
conslistp = (reserved ":") `semi` (ppure (App `c'` (App cons)) )

termp :: Parser Expr
termp = litintp
    `por` letp
    `por` casep
    `por` varp
    `por` conp
    `por` braces lamp
    `por` parens exprp
    `por` brackets listp

litintp :: Parser Expr
litintp = number `pbind` (\n-> ppure (nat n) )

letp :: Parser Expr
letp =
  reserved "let" `semi` (
  bindsp `pbind` (\binds->
  reserved "in" `semi` (
  exprp `pbind` (\inexpr->
  ppure (Let binds inexpr)
  ))))

bindsp :: Parser [Binding]
bindsp =
        varnamep `pbind` (\var->
        reserved "=" `semi` (
        exprp `pbind` (\valexpr->
        (
          reserved ";" `semi` (
          bindsp `pbind` (\binds->
          ppure ((var, valexpr):binds) ) )) `por`
          ppure [(var, valexpr)]
        )))

varp :: Parser Expr
varp = varnamep `pbind` (\var->ppure (Var var))

conp :: Parser Expr
conp = upperword `pbind` (\tag-> ppure (con tag) )

lamp :: Parser Expr
lamp =
  varnamep `pbind` (\var->
  reserved "->" `semi` (
  exprp `pbind` (\valexpr->
  ppure (Lam var valexpr)
  )))

casep :: Parser Expr
casep =
  reserved "case" `semi` (
  exprp `pbind` (\scexpr->
  reserved "of" `semi` (
  some altp `pbind` (\alts->
  ppure (Case scexpr alts)
  ))))

altp :: Parser (Pat, Expr)
altp =
  upperword `pbind` (\tag->
  many varnamep `pbind` (\vars->
  reserved "->" `semi` (
  exprp `pbind` (\res->
  reserved ";" `semi` (
  ppure (Pat tag vars, res)
  )))))

listp :: Parser Expr
listp = (exprp `pbind` (\item->
      (
        reserved "," `semi` (
        listp `pbind` (\rest->
        ppure (app cons [item, rest])))) `por`
        ppure (app cons [item, nil])
    )) `por` ppure nil


isKeyword :: String -> Bool
isKeyword s = isElem s ["let", "in", "case", "of"]

varnamep :: Parser Var
varnamep = sat ("let, in, case, of") varid (not' `c'` isKeyword)

varid :: Parser Var
varid z = (loweralpha `por` dollar `por` underscore `pbind` (\c->
  many (alpha `por` digit `por` underscore `por` quote) `pbind` (\cs->
  spaces `semi` (
  ppure (c:cs)
  )))) z

isNil :: [a] -> Bool
isNil xs = case xs of
  [] -> True
  (x':xs') -> False

parseWith :: Show a => Parser a -> String -> a
parseWith p s =
  case (spaces `semi` p) s of
    Done a chars rest -> if isNil rest then a else error (
      "Parser didn't consume entire stream: <<" `app'` rest `app'` ">> " `app'`
      " in <<" `app'` s `app'` ">> at " `app'` show chars `app'`
                   " with " `app'` show a)
    Error msg  -> error ("Parser error: " `app'` msg `app'` s)

parseExpr :: String -> Expr
parseExpr z = parseWith exprp z

root :: String -> Expr
root z = parseExpr z
