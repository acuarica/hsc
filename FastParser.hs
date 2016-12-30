
module FastParser (parseExpr) where

import Expr (Expr(Var, Lam, App, Let, Case), Var, Binding, Pat(Pat),
  con, app, zero, suc, nil, cons, nat)
import Data.Char (isDigit, isAlpha, isLower, isUpper)

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
  Done a chars rest -> case f a rest of
    Error msg' -> Error msg'
    Done b chars' rest' -> Done b (chars+chars') rest'

semi :: Parser a -> Parser b -> Parser b
semi p q = pbind p (\_x -> q)

por :: Parser a -> Parser a -> Parser a
por p q s = case p s of
  Error msg'p -> case q s of
    Error msg'q -> Error (msg'p ++ " or " ++ msg'q)
    Done a'q c'q r'q -> Done a'q c'q r'q
  Done a'p c'p r'p -> Done a'p c'p r'p

some :: Parser a -> Parser [a]
some v = pseq (pmap (:) v) (many v)

many :: Parser a -> Parser [a]
many v = some v `por` ppure []

char :: Char -> Parser Char
char x s = case s of []->Error "EOF"; (c:cs)->if c==x then Done c 1 cs else Error [c]

natural :: String -> ParserResult Integer
natural s = case (digits s) of Error m -> Error m; Done ds c r -> Done (read ds) c r
  where
    digits s = case digit s of
      Error msg -> Error msg
      Done d chars rest -> case digits rest of
        Error msg'p -> Done [d] 0 rest
        Done a'p c'p r'p -> Done (d:a'p) c'p r'p

string :: String -> Parser String
string xs s = case xs of
  [] -> Done [] 0 s
  (c:cs) -> case (char c s) of
    Error msg -> Error msg
    Done _ chars rest -> case (string cs rest) of
      Error msg' -> Error msg'
      Done _ chars'' rest'' -> Done (c:cs) (chars+chars'') rest''

token :: Parser a -> Parser a
token p =  p `pbind` (\a-> spaces `semi` ppure a)

reserved :: String -> Parser String
reserved t = token (string t)

spaces :: Parser String
spaces s = case s of
    [] -> Done [] 0 s
    (c:cs) -> if c == ' ' || c == '\n' || c == '\r'
      then case spaces cs of
             Done cs' chars' rest' -> Done (c:cs') (1+chars') rest'
      else Done [] 0 s

digit :: Parser Char
digit s =case s of []->Error "EOF";(c:cs)->if isDigit c then Done c 1 cs else Error "d"

alpha :: Parser Char
alpha s =case s of []->Error "EOF";(c:cs)->if isAlpha c then Done c 1 cs else Error "a"

loweralpha :: Parser Char
loweralpha s = case s of
  [] -> Error "EOF"
  (c:cs) -> if isAlpha c && isLower c then Done c 1 cs else Error "l"

upperalpha :: Parser Char
upperalpha s = case s of
  [] -> Error "EOF"
  (c:cs) -> if isAlpha c && isUpper c then Done c 1 cs else Error "l"

dollar :: Parser Char
dollar s = case s of []->Error "EOF"; (c:cs)->
                       if c=='$' then Done '$' 1 cs else Error "$"

underscore :: Parser Char
underscore s=case s of []->Error"EOF";(c:cs)->
                         if c=='_' then Done '_' 1 cs else Error "_"

quote :: Parser Char
quote s = case s of []->Error "EOF";(c:cs)->
                      if c=='\'' then Done '\'' 1 cs else Error "\'"

string'minus'or :: Parser String
string'minus'or s = case s of
  [] -> Done "" 0 s
  (c:cs) -> if c=='-'
    then Done "-" 1 cs
    else Done "" 0 s

number :: Parser Int
number =
  string'minus'or `pbind` \s->
  some digit `pbind` \cs->
  spaces `semi`
  ppure (read (s ++ cs))

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
conslistp = (reserved ":") `semi` (ppure (App . (App cons)) )

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

varnamep :: Parser Var
varnamep s = case varid s of
    Error msg -> Error msg
    Done v chars rest -> if v=="let" || v== "in" || v=="case" || v=="of"
      then Error "let|in|case|of"
      else Done v chars rest

varid :: Parser Var
varid z = (loweralpha `por` dollar `por` underscore `pbind` (\c->
  many (alpha `por` digit `por` underscore `por` quote) `pbind` (\cs->
  spaces `semi` (
  ppure (c:cs)
  )))) z

parseExpr :: String -> Expr
parseExpr s = case (spaces `semi` exprp) s of
    Done a c rest -> if null rest then a else error "Parser not consume whole stream"
    Error msg -> error "Parser error"
