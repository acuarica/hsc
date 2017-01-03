
module FastParser (parseExpr) where

import Prelude hiding ((>>=), (>>))
import Expr (Expr(Var, Con, Lam, App, Let, Case), Var, Binding, Pat(Pat))
import Data.Char (isDigit, isAlpha, isLower, isUpper)

data ParserResult a = Error String | Done a Int String
type Parser a = String -> ParserResult a

(>>=) :: Parser a -> (a -> Parser b) -> Parser b
(>>=) p f s = case p s of
  Error msg -> Error msg
  Done a chars rest -> case f a rest of
    Error msg' -> Error msg'
    Done b chars' rest' -> Done b (chars+chars') rest'

(>>) :: Parser a -> Parser b -> Parser b
(>>) p q = p >>= (\_x -> q)

eeof :: ParserResult a
eeof = Error "EOF"

reserved'semi s=case s of
  [] -> Error "EOF"
  (c:cs)->if c==';'then spaces'void cs else Error [c]

reserved'equal s=case s of
  []->eeof;(c:cs)->if c=='='then spaces'void cs else Error [c]

reserved'arrow s=case s of
  []->eeof
  (c:cs)->if c=='-'
    then case cs of
           [] -> eeof
           (c':cs') -> if c'=='>'
             then spaces'void cs'
             else Error [c']
    else Error [c]

spaces'void :: String -> ParserResult ()
spaces'void s = case s of
  [] -> Done () 0 s
  (c:cs) -> if c == ' ' || c == '\n' || c=='\r'
    then spaces'void cs
    else Done () 0 s

many'alpha s = case s of
  [] -> Done [] 0 s
  (c:cs) -> if isAlpha c
    then case many'alpha cs of
      Done a'p c'p r'p -> Done (c:a'p) c'p r'p
    else Done [] 0 s

restl p op a s = case (op >>= (\f-> p >>= (\b-> restl p op (f a b) ))) s of
  Error _ -> Done a 0 s
  done -> done

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op = p >>= (\a->restl p op a)

restr p op a s =case (op>>=(\f->p>>=(\b->restr p op b>>=(\b'->Done (f a b') 0 )))) s of
  Error _ -> Done a 0 s
  done -> done

chainr1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainr1 p op = p >>= (\a-> restr p op  a)

exprp :: Parser Expr
exprp = (termp `chainr1` conslistp) `chainl1` (Done App 0)

conslistp :: Parser (Expr -> Expr -> Expr)
conslistp s = case reserved'colon s of
  Error msg -> Error msg
  Done _ chars rest -> Done (App . (App (Con "Cons" []))) chars rest
  where
    reserved'colon s = case s of
      [] -> Error "EOF"
      (c:cs) -> if c==':'then spaces'void cs else Error [c]

(<|>) :: Parser a -> Parser a -> Parser a
(<|>) p q s = case p s of
  Error msg'p -> case q s of
    Error msg'q -> Error (msg'p ++ " or " ++ msg'q)
    Done a'q c'q r'q -> Done a'q c'q r'q
  Done a'p c'p r'p -> Done a'p c'p r'p

braces'parens'brackets s = case s of
  [] -> Error "EOF or EOF or EOF or EOF or EOF or EOF or EOF"
  c:cs ->
    if (isAlpha c && isLower c) || c == '$' || c == '_' then
        case many'alpha'digit'underscore'quote cs of
           Done as chars rest -> case spaces'void rest of
             Done _ chars' s'-> let v = c:as in
               if v=="let" || v== "in" || v=="case" || v=="of"
                then Error "let|in|case|of"
               else Done (Var v) (chars+chars') s'
    else if isAlpha c && isUpper c then case many'alpha cs of
      Done cs chars' rest' -> case spaces'void rest' of
        Done _ chars'' rest'' -> Done (Con (c:cs) []) (1+chars'') rest''
    else if c == '{'then (spaces'void>> lamp >>= (\n-> reserved'cb >> Done n 0)) cs
    else if c=='('then (spaces'void >> exprp >>= (\n-> reserved'cp >> Done n 0)) cs
    else if c=='['then (spaces'void >> listp >>= (\n-> reserved'cs >> Done n 0)) cs
    else Error ['l', c, c, c, c, c, c]

reserved'cb s=case s of[]->eeof;(c:cs)->if c=='}'then spaces'void cs else Error [c]
reserved'cp s=case s of[]->eeof;(c:cs)->if c==')'then spaces'void cs else Error [c]
reserved'cs s=case s of[]->eeof;(c:cs)->if c==']'then spaces'void cs else Error [c]

termp :: Parser Expr
termp = litintp
    <|> letp
    <|> casep
    <|> braces'parens'brackets

litintp :: Parser Expr
litintp =
  string'minus'or >>= \sign->
  some'digit >>= \ds->
  \s' -> case spaces'void s' of Done _ c' r -> Done (nat (read (sign ++ ds))) c' r
  where
    nat n = if n > 0 then App (Con "Succ" []) (nat (n - 1)) else Con "Zero" []
    string'minus'or s = case s of
      [] -> Done "" 0 s
      c:cs -> if c == '-' then Done "-" 1 cs else Done "" 0 s
    some'digit s = case s of
      [] -> Error "EOF"
      (d:rest) -> if isDigit d
        then case some'digit rest of
              Error msg'p -> Done [d] 0 rest
              Done a'p c'p r'p -> Done (d:a'p) c'p r'p
        else Error "d"

letp :: Parser Expr
letp =
  reserved'et >> (
  bindsp >>= (\binds->
  reserved'in >> (
  exprp >>= (\inexpr->
  Done (Let binds inexpr) 0
  ))))
  where
    reserved'et s = case s of
      []->eeof
      (c:cs)->if c=='l' then
        case cs of
              [] -> Error "EOF"
              (c':cs') -> if c'=='e'
                then case cs' of
                        [] -> Error "EOF"
                        (c'':cs'') -> if c''=='t'
                          then spaces'void cs''
                          else Error [c'']
                else Error [c']
        else Error [c]
    reserved'in s=case s of
      []->Error "EOF"
      (c:cs)->if c=='i'
        then case cs of
              [] -> Error "EOF"
              (c':cs') -> if c'=='n'
                then spaces'void cs'
                else Error [c']
        else Error [c]

bindsp :: Parser [Binding]
bindsp =
        varnamep >>= (\var->
        reserved'equal >> (
        exprp >>= (\valexpr->
        \s-> case (
          reserved'semi >> (
          \s'->case bindsp s' of
            Done binds c' s''-> Done ((var, valexpr):binds) c' s''
            err -> err
          )) s of
          Error _ -> Done [(var, valexpr)] 0 s
          done -> done
        )))

lamp :: Parser Expr
lamp =
  varnamep >>= (\var->
  reserved'arrow >> (
  \s->case exprp s of Done valexpr c' r'-> Done (Lam var valexpr) c' r'; err->err
  ))

casep :: Parser Expr
casep =
  reserved'ase >> (
  exprp >>= (\sc->
  reserved'of >>
  \s -> case some'altp s of
    Error m -> Error m
    Done alts c' r' -> Done (Case sc alts) c' r'
  ))
  where
    some'altp s = case altp s of
      Error msg -> Error msg
      Done alt c' s' -> case some'altp s' of
        Error msg' -> Done [alt] c' s'
        Done alts c'' s'' -> Done (alt:alts) (c'+c'') s''
    reserved'of s = case s of
      []->eeof
      (c:cs)->if c=='o'
        then case cs of
              [] -> eeof
              (c':cs') -> if c'=='f'
                then spaces'void cs'
                else Error [c']
        else Error [c]
    reserved'ase s = case s of
      []->eeof
      (c:cs)->if c=='c' then
          case cs of
              [] -> Error "EOF"
              (c':cs') -> if c'=='a'
                then case cs' of
                        [] -> Error "EOF"
                        (c'':cs'') -> if c''=='s'
                          then case cs'' of
                                [] -> Error "EOF"
                                (c''':cs''') -> if c'''=='e'
                                  then spaces'void cs'''
                                  else Error [c''']
                          else Error [c'']
                else Error [c']
       else Error [c]

altp :: Parser (Pat, Expr)
altp =
  upperword >>= (\tag->
  many'varnamep >>= (\vars->
  reserved'arrow >> (
  exprp >>= (\alte->
  (\s->case reserved'semi s of
      Error m->Error m;Done _ c t -> Done (Pat tag vars, alte) c t
  )))))
  where
    many'varnamep s = case varnamep s of
      Error msg -> Done [] 0 s
      Done v c' s' -> case many'varnamep s' of
        Done vs c'' s'' -> Done (v:vs) (c'+c'') s''
    upperword s = case s of
      [] -> Error "EOF"
      (c:rest) -> if isAlpha c && isUpper c
        then case many'alpha rest of
          Done cs chars' rest' -> case spaces'void rest' of
            Done _ chars'' rest'' -> Done (c:cs) (1+chars'') rest''
          else Error "l"

listp :: String -> ParserResult Expr
listp s = case (exprp >>= (\item-> (\s' -> case (
        reserved'comma >> (
        listp >>= (\rest->
        Done (  App (App (Con "Cons" []) item) rest  ) 0 ))) s' of
          Error _ -> Done (  App (App (Con "Cons" []) item) (Con "Nil" []) ) 0 s'
          done -> done)
    )) s of
            Error _ -> Done (Con "Nil" []) 0 s
            done -> done
  where
    reserved'comma s=case s of
      []->Error "EOF";(c:cs)->if c==','then spaces'void cs else Error [c]

varnamep :: String -> ParserResult String
varnamep s = case varid s of
    Error msg -> Error msg
    Done v chars rest -> if v=="let" || v== "in" || v=="case" || v=="of"
      then Error "let|in|case|of"
      else Done v chars rest

varid :: String -> ParserResult Var
varid z = case z of
  [] -> Error "EOF"
  (c:cs') -> if (isAlpha c && isLower c) || c == '$' || c == '_'
    then case many'alpha'digit'underscore'quote cs' of
           Done as chars rest -> case spaces'void rest of
             Done _ chars s'-> Done (c:as) chars s'
    else Error [c]

many'alpha'digit'underscore'quote :: String -> ParserResult String
many'alpha'digit'underscore'quote s = case s of
  [] -> Done [] 0 s
  (c:cs) -> if isAlpha c || isDigit c || c == '_' || c == '\''
    then case many'alpha'digit'underscore'quote cs of
      Done a'p c'p r'p -> Done (c:a'p) c'p r'p
    else Done [] 0 s

parseExpr :: String -> Expr
parseExpr s = case spaces'void s of
  Done _ chars rest -> case exprp rest of
    Error msg -> error "Parser error"
    Done e c rest' -> if null rest'
      then e
      else error "Parser not consume whole stream"
