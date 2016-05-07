
module Pretty () where

import Control.Exception (assert)
import Data.Maybe (fromMaybe)
import Data.List (intercalate)

import Expr

instance Show Expr where
  show = pretty

-- | Pretty prints an expression.
pretty :: Expr -> String
pretty = pretty' False

-- | Pretty prints an expression.
-- | par indicates whether parenthesis are needed.
pretty' :: Bool -> Expr -> String
pretty' par expr = case expr of
  Var var tainted ->
    prettyTainted tainted : var
  Con tag args -> fromMaybe (if null args
      then tag
      else paren (tag ++ " " ++ unwords (map (pretty' True) args)) )
      ((prettyNat <|> prettyList) expr)
  Lam var expr ->
    "{" ++ var ++ " -> " ++ pretty expr ++ "}"
  Let var valexpr inexpr ->
    "let " ++ var ++ "=" ++ pretty valexpr ++ " in " ++ pretty inexpr
  App funexpr valexpr ->
     paren (pretty funexpr ++ " " ++ pretty' True valexpr)
  Case scexpr cs tainted ->
    "case" ++ prettyTainted tainted : ' ' : pretty scexpr ++ " of " ++
    foldr (\ (p, e) s -> pretty p ++ "->" ++ pretty e ++ "; " ++ s) "" cs
  where paren s = if par then "(" ++ s ++ ")" else s

prettyTainted :: Bool -> Char
prettyTainted tainted = if tainted then '!' else '?'

type PrettyCon = Expr -> Maybe String

(<|>) :: PrettyCon -> PrettyCon -> PrettyCon
f <|> g = \expr -> case f expr of
  Nothing -> g expr
  Just s -> Just s

prettyNat :: Expr -> Maybe String
prettyNat expr = case doNat expr of
  (n, Nothing) -> Just (show n)
  (0, Just expr') -> Nothing
  (n, Just expr') -> Just (show n ++ "@" ++ pretty' True expr')

prettyList :: Expr -> Maybe String
prettyList expr = case doList expr of
  (xs, Nothing) -> Just ("[" ++ int ", " xs ++ "]")
  ([], Just _) -> Nothing
  (xs, Just expr) -> Just ("(" ++ int ":" xs ++ ":" ++ pretty expr ++ ")")
  where int sep xs = intercalate sep (map pretty xs)

doNat :: Expr -> (Int, Maybe Expr)
doNat expr = case expr of
  Con "Zero" args -> assert (null args) (0, Nothing)
  Con "Succ" args -> case args of
    [] -> (0, Just expr)
    [arg] -> case doNat arg of
      (n, e) -> (n+1, e)
    _ -> error "Invalid arguments for Succ"
  expr' -> (0, Just expr)

doList :: Expr -> ([Expr], Maybe Expr)
doList expr = case expr of
  Con "Nil" args -> assert (null args) ([], Nothing)
  Con "Cons" args -> case args of
    [item, rest] -> case doList rest of
      (xs, e) -> (item:xs, e)
    _ -> ([], Just expr)
  expr' -> ([], Just expr)
