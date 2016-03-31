{-# LANGUAGE InstanceSigs  #-}

module ParserCombinator where

import Data.Char
import Control.Monad
import Control.Applicative
import Text.Printf

-- | Represents how many characters are consumed within the parsed string.
type Chars = Int

-- | The result of parsing. It can be an Error or Done.
-- | Done contains the value, line, column and the tail to be parsed.
data ParserResult a = Error String | Done a Chars String

-- | The Parser type
newtype Parser a = Parser { parse :: String -> ParserResult a }

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f (Parser parse) = Parser (\s ->
    case parse s of
      Error msg -> Error msg
      Done a chars rest -> Done (f a) chars rest
    )

instance Applicative Parser where
  pure :: a -> Parser a
  pure a = Parser (Done a 0)

  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  (Parser pf) <*> (Parser p) = Parser (\s ->
    case pf s of
      Error msg -> Error msg
      Done f chars rest -> case p rest of
        Error msg' -> Error msg'
        Done a chars' rest' -> Done (f a) (chars+chars') rest'
    )

instance Monad Parser where
  return :: a -> Parser a
  return = pure

  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  (>>=) (Parser p) f = Parser (\s ->
    case p s of
      Error msg -> Error msg
      Done a chars rest -> case parse (f a) rest of
        Error msg' -> Error msg'
        Done b chars' rest' -> Done b (chars+chars') rest'
    )

instance Alternative Parser where
  empty :: Parser a
  empty = Parser (const (Error "Empty parser"))

  (<|>) :: Parser a -> Parser a -> Parser a
  (<|>) (Parser p) (Parser q) = Parser (\s ->
      case p s of
        Error msg -> q s
        res -> res
    )

item :: Parser Char
item = Parser (\s -> case s of
    []     -> Error "Reached EOF"
    (c:cs) -> Done c 1 cs)

satisfy :: String -> (Char -> Bool) -> Parser Char
satisfy msg pred = (>>=) item (\c ->
  if pred c
    then pure c
    else Parser (const (Error (printf "expecting %s got %c" msg c))))

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

str s = "``" ++ s ++ "''"

parseWith :: Show a => Parser a -> String -> a
parseWith p s =
  case parse (do { spaces; p }) s of
    Done a _ [] -> a
    Done a chars rest -> error $ "Parser didn't consume entire stream: "++
      str rest ++ " in " ++ str s ++ " at " ++ show chars ++
                   " with " ++ show a
    Error msg  -> error $ printf "Parser error: %s in ``%s''" msg s
