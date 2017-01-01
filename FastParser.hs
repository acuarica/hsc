
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

some'digit s = case digit s of
  Error msg -> Error msg
  Done d chars rest -> case some'digit rest of
    Error msg'p -> Done [d] 0 rest
    Done a'p c'p r'p -> Done (d:a'p) c'p r'p

eeof :: ParserResult a
eeof = Error "EOF"

reserved'semi :: String -> ParserResult ()
reserved'semi s=case s of []->eeof;(c:cs)->if c==';'then spaces'void cs else Error [c]
reserved'colon s=case s of []->eeof;(c:cs)->if c==':'then spaces'void cs else Error [c]
reserved'comma s=case s of []->eeof;(c:cs)->if c==','then spaces'void cs else Error [c]
reserved'equal s=case s of []->eeof;(c:cs)->if c=='='then spaces'void cs else Error [c]

reserved'arrow s=case s of
  []->eeof
  (c:cs)->if c=='-'
    then case cs of
           [] -> eeof
           (c':cs') -> if c'=='>'
             then spaces'void cs'
             else Error [c']
    else Error [c]

reserved'in s=case s of
  []->eeof
  (c:cs)->if c=='i'
    then case cs of
           [] -> eeof
           (c':cs') -> if c'=='n'
             then spaces'void cs'
             else Error [c']
    else Error [c]

reserved'of s=case s of
  []->eeof
  (c:cs)->if c=='o'
    then case cs of
           [] -> eeof
           (c':cs') -> if c'=='f'
             then spaces'void cs'
             else Error [c']
    else Error [c]

reserved'let s=case s of
  []->eeof
  (c:cs)->if c=='l'
    then case cs of
           [] -> eeof
           (c':cs') -> if c'=='e'
             then case cs' of
                    [] -> eeof
                    (c'':cs'') -> if c''=='t'
                      then spaces'void cs''
                      else Error [c'']
             else Error [c']
    else Error [c]

reserved'case s=case s of
  []->eeof
  (c:cs)->if c=='c'
    then case cs of
           [] -> eeof
           (c':cs') -> if c'=='a'
             then case cs' of
                    [] -> eeof
                    (c'':cs'') -> if c''=='s'
                      then case cs'' of
                             [] -> eeof
                             (c''':cs''') -> if c'''=='e'
                               then spaces'void cs'''
                               else Error [c''']
                      else Error [c'']
             else Error [c']
    else Error [c]

spaces'void :: String -> ParserResult ()
spaces'void s = case s of
    [] -> Done () 0 s
    (c:cs) -> if c == ' ' || c == '\n' || c=='\r' then spaces'void cs else Done () 0 s

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

string'minus'or :: Parser String
string'minus'or s = case s of
  [] -> Done "" 0 s
  (c:cs) -> if c=='-'
    then Done "-" 1 cs
    else Done "" 0 s

many'alpha s = case s of
  [] -> Done [] 0 s
  (c:cs) -> if isAlpha c
    then case many'alpha cs of
      Done a'p c'p r'p -> Done (c:a'p) c'p r'p
    else Done [] 0 s

lowerword :: Parser String
lowerword =
  loweralpha `pbind` \c->
  many'alpha `pbind` \cs ->
  spaces'void `semi`
  ppure (c:cs)

upperword :: Parser String
upperword =
  upperalpha `pbind` \c->
  many'alpha `pbind` \cs->
  spaces'void `semi`
  ppure (c:cs)

reserved'op s=case s of []->eeof;(c:cs)->if c=='('then spaces'void cs else Error [c]
reserved'cp s=case s of []->eeof;(c:cs)->if c==')'then spaces'void cs else Error [c]
reserved'ob s=case s of []->eeof;(c:cs)->if c=='{'then spaces'void cs else Error [c]
reserved'cb s=case s of []->eeof;(c:cs)->if c=='}'then spaces'void cs else Error [c]
reserved'os s=case s of []->eeof;(c:cs)->if c=='['then spaces'void cs else Error [c]
reserved'cs s=case s of []->eeof;(c:cs)->if c==']'then spaces'void cs else Error [c]

parens :: Parser a -> Parser a
parens m = reserved'op `semi` m `pbind` (\n-> reserved'cp  `semi` ppure n )

braces :: Parser a -> Parser a
braces m = reserved'ob `semi` m `pbind` (\n-> reserved'cb `semi` ppure n )

brackets :: Parser a -> Parser a
brackets m = reserved'os `semi` m `pbind` (\n-> reserved'cs `semi` ppure n )

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
conslistp = reserved'colon `semi` (ppure (App . (App cons)) )

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
litintp =
  string'minus'or `pbind` \s->
  some'digit `pbind` \cs->
  spaces'void `semi`
  ppure (nat (read (s ++ cs)))

letp :: Parser Expr
letp =
  reserved'let  `semi` (
  bindsp `pbind` (\binds->
  reserved'in `semi` (
  exprp `pbind` (\inexpr->
  ppure (Let binds inexpr)
  ))))

bindsp :: Parser [Binding]
bindsp =
        varnamep `pbind` (\var->
        reserved'equal `semi` (
        exprp `pbind` (\valexpr->
        (
          reserved'semi `semi` (
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
  reserved'arrow `semi` (
  exprp `pbind` (\valexpr->
  ppure (Lam var valexpr)
  )))

casep :: Parser Expr
casep =
  reserved'case `semi` (
  exprp `pbind` (\scexpr->
  reserved'of `semi` (
  some altp `pbind` (\alts->
  ppure (Case scexpr alts)
  ))))

altp :: Parser (Pat, Expr)
altp =
  upperword `pbind` (\tag->
  many varnamep `pbind` (\vars->
  reserved'arrow `semi` (
  exprp `pbind` (\res->
  reserved'semi `semi` (
  ppure (Pat tag vars, res)
  )))))

listp :: Parser Expr
listp = (exprp `pbind` (\item->
      (
        reserved'comma `semi` (
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

alpha'digit'underscore'quote :: String -> ParserResult Char
alpha'digit'underscore'quote s = case s of
  [] -> Error "EOF"
  (c:cs) -> if isAlpha c || isDigit c || c == '_' || c == '\''
    then Done c 1 cs
    else Error [c]

loweralpha'dollar'underscore :: String -> ParserResult Char
loweralpha'dollar'underscore s = case s of
  [] -> Error "EOF"
  (c:cs) -> if (isAlpha c && isLower c) || c == '$' || c == '_'
    then Done c 1 cs
    else Error [c]

varid :: Parser Var
varid z = (loweralpha'dollar'underscore `pbind` (\c->
  many (alpha'digit'underscore'quote) `pbind` (\cs->
  spaces'void `semi` (
  ppure (c:cs)
  )))) z

parseExpr :: String -> Expr
parseExpr s = case (spaces'void `semi` exprp) s of
  Done a c rest -> if null rest then a else error "Parser not consume whole stream"
  Error msg -> error "Parser error"
