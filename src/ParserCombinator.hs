
module ParserCombinator where

import Data.Char
import Control.Monad
import Control.Applicative

str s = "``" ++ s ++ "''"

newtype Parser a = Parser { parse :: String -> [(a, Int, String)] }

apply :: (a -> b) -> Int -> (a, Int, String) -> (b, Int, String)
apply f m (a, n, b) = (f a, n + m, b)

item :: Parser Char
item = Parser (\s -> case s of
    []     -> []
    (c:cs) -> [(c, 1, cs)])

instance Functor Parser where
  fmap f (Parser cs) = Parser (map (apply f 0) . cs)

instance Applicative Parser where
  pure a = Parser (\s -> [(a, 0, s)])
  (Parser cs1) <*> (Parser cs2) = Parser $ \s ->
    [(f a, n1+n2, s2) | (f, n1, s1) <- cs1 s, (a, n2, s2) <- cs2 s1]

instance Monad Parser where
  return = pure
  (>>=) p f = Parser $ \s ->
        concatMap (\(a, n, s') -> map (apply id n) (parse (f a) s') )
                  (parse p s)

instance Alternative Parser where
  empty = Parser (const [])
  (<|>) p q = Parser (\s -> case parse p s of
      []     -> parse q s
      res    -> res)

satisfy :: (Char -> Bool) -> Parser Char
satisfy pred = (>>=) item (\c -> if pred c then pure c else empty)

oneOf :: String -> Parser Char
oneOf s = satisfy (`elem` s)

chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl p op a = (p `chainl1` op) <|> return a

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainl1` op = do {a <- p; rest a}
  where rest a = (do f <- op
                     b <- p
                     rest (f a b))
                 <|> return a

char :: Char -> Parser Char
char c = satisfy (c ==)

natural :: Parser Integer
natural = fmap read (some (satisfy isDigit))

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
digit = satisfy isDigit

alpha :: Parser Char
alpha = satisfy isAlpha

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
    [(res, _, [])] -> res
    [(res, n, rs)] -> error $ "Parser didn't consume entire stream: " ++
                   str rs ++ " in " ++ str s ++ " at " ++ show n ++
                   " with " ++ show res
    res           -> error $ "Parser error " ++ show res ++
                    " in " ++ str s
