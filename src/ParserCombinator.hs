
module ParserCombinator where

import Data.Char
import Control.Monad
import Control.Applicative
import Text.Printf

str s = "``" ++ s ++ "''"

data ParserResult a = Error String | Ok (a, Int, String)

newtype Parser a = Parser { parse :: String -> ParserResult a }

item :: Parser Char
item = Parser (\s -> case s of
    []     -> Error "Reached EOF"
    (c:cs) -> Ok (c, 1, cs))

instance Functor Parser where
  fmap f (Parser p) = Parser (\ s ->
    case p s of
      Error msg    -> Error msg
      Ok (a, n, s) -> Ok (f a, n, s))

instance Applicative Parser where
  pure a = Parser (\s -> Ok (a, 0, s))
  (Parser p1) <*> (Parser p2) = Parser ( \s ->
    case p1 s of
      Error msg -> Error msg
      Ok (f, n1, s1) -> case p2 s1 of
        Error msg -> Error msg
        Ok (a, n2, s2) -> Ok (f a, n1+n2, s2) )

instance Monad Parser where
  return = pure
  (>>=) p f = Parser ( \s ->
        case parse p s of
          Error msg -> Error msg
          Ok (a, n, s) -> case parse (f a) s of
              Error msg -> Error msg
              Ok (a', n', s') -> Ok (a', n+n', s') )

instance Alternative Parser where
  empty = Parser (const (Error "empty alternative parser"))
  (<|>) p q = Parser (\s -> case parse p s of
      Error _ -> parse q s
      res     -> res)

satisfy :: String -> (Char -> Bool) -> Parser Char
satisfy msg pred = (>>=) item (\c ->
  if pred c
    then pure c
    else Parser (const (Error (printf "expecting %s got %c" msg c))) )

oneOf :: String -> Parser Char
oneOf s = satisfy s (`elem` s)

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainl1` op = do {a <- p; rest a}
  where rest a = (do f <- op
                     b <- p
                     rest (f a b))
                 <|> return a

char :: Char -> Parser Char
char c = satisfy [c] (c ==)

--natural :: Parser Integer
--natural = fmap read (some (satisfy "isDigit" isDigit))

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

number :: Parser Int
number = do
  s <- string "-" <|> return []
  cs <- some digit
  spaces
  return $ read (s ++ cs)

word :: Parser String
word = do
  cs <- some alpha
  spaces
  return cs

parens :: Parser a -> Parser a
parens m = do { reserved "("; n <- m; reserved ")"; return n }

braces :: Parser a -> Parser a
braces m = do { reserved "{"; n <- m; reserved "}"; return n }

parseWith :: Show a => Parser a -> String -> a
parseWith p s =
  case parse (do { spaces; p }) s of
    Ok (res, _, []) -> res
    Ok (res, n, rs) -> error $ "Parser didn't consume entire stream: "++
      str rs ++ " in " ++ str s ++ " at " ++ show n ++
                   " with " ++ show res
    Error msg  -> error $ printf "Parser error: %s in ``%s''" msg s
