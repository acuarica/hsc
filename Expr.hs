{-# LANGUAGE FlexibleInstances #-}

module Expr (
  Expr(..), Var, Pat (Pat),
  con, app, appVars,
  subst, substAlts, lookupAlt, freeVars,
  zero, suc, nil, cons
) where

import Control.Arrow (second)
import Data.Maybe (fromMaybe)
import Control.Exception (assert)
import Data.List (nub, delete, (\\), union, intercalate)

-- | The expression type.
data Expr
  = Var  Var
  | Con  Tag  [Expr]
  | Let  Var  Expr Expr
  | Lam  Var  Expr
  | App  Expr Expr
  | Case Expr [(Pat, Expr)]
  deriving Eq

-- | Represents identifier variable.
type Var = String

-- | Represents constructor name.
-- | Also called tag to be matched in case expressions.
type Tag = String

-- | Case patterns against tag.
data Pat = Pat Tag [Var] deriving Eq

-- | Creates a constructor with the given tag.
con :: Tag -> Expr
con tag = Con tag []

-- | Application of args to expr.
app :: Expr -> [Expr] -> Expr
app expr args = case args of
  [] -> expr
  arg:args' -> app (App expr arg) args'

-- | Application of variables to an expression.
appVars :: Expr -> [Var] -> Expr
appVars expr = app expr . map Var

-- | Variable substitution.
subst :: (Var, Expr) -> Expr -> Expr
subst (var, valexpr) bodyexpr = case bodyexpr of
  Var var' -> if var' == var then valexpr else Var var'
  _ -> apply (subst (var, valexpr)) bodyexpr

-- | Subtitutes a list of bindings in bodyexpr.
substAlts :: [(Var, Expr)] -> Expr -> Expr
substAlts bindings bodyexpr = foldl (flip subst) bodyexpr bindings

-- | Lookup the alternative according to the constructor tag.
lookupAlt :: Tag -> [(Pat, Expr)] -> (Pat, Expr)
lookupAlt tag ((Pat pattag patvars, expr):alts) = if pattag == tag
  then (Pat pattag patvars, expr)
  else lookupAlt tag alts

-- | Free variables of an expression.
freeVars :: Expr -> [Var]
freeVars expr = case expr of
  Var var -> [var]
  Con _ args -> nub (concatMap freeVars args)
  Lam var lamexpr -> delete var (freeVars lamexpr)
  Let var valexpr inexpr ->
    delete var (freeVars inexpr `union` freeVars valexpr)
  App funexpr valexpr -> freeVars funexpr `union` freeVars valexpr
  Case scexpr alts -> nub (freeVars scexpr ++
    concatMap (\(Pat p vars, e) -> freeVars e \\ vars) alts)

apply :: (Expr -> Expr) -> Expr -> Expr
apply f expr = case expr of
  Var var -> Var var
  Con tag args -> Con tag (map f args)
  Lam var lamexpr -> Lam var (f lamexpr)
  Let var valexpr inexpr -> Let var (f valexpr) (f inexpr)
  App funexpr valexpr -> App (f funexpr) (f valexpr)
  Case scexpr alts -> Case (f scexpr) (map (second f) alts)

-- | Some common used expressions for easy write of expressions.
-- | These expressions are pretty printed accordingly.
zero, suc, nil, cons :: Expr
zero = con "Zero"
suc = con "Succ"
nil = con "Nil"
cons = con "Cons"

instance Show Expr where
  show = show' False
    where
    show' par expr = case expr of
      Var var ->
        var
      Con tag args -> fromMaybe (if null args
        then tag
        else paren par (tag ++ " " ++ unwords (map (show' True) args)))
          ((prettyNat <|> prettyList) expr)
      Lam var expr ->
        "{" ++ var ++ "->" ++ show expr ++ "}"
      Let var valexpr inexpr ->
        paren par ("let " ++ var ++ "=" ++ show' True valexpr ++ "" ++
               " in " ++ show inexpr)
      App funexpr valexpr ->
         paren par (show funexpr ++ " " ++ show' True valexpr)
      Case scexpr cs ->
        "case " ++ show scexpr ++ " of " ++
        foldr (\ (p, e) s -> show p ++ "->" ++ show e ++ ";" ++ s) "" cs
    paren par s = if par then "(" ++ s ++ ")" else s
    prettyNat expr = case doNat expr of
      (n, Nothing) -> Just (show n)
      (0, Just expr') -> Nothing
      (n, Just expr') -> Just (show n ++ "@" ++ show' True expr')
    prettyList expr = case doList expr of
      (xs, Nothing) -> Just ("[" ++ int ", " xs ++ "]")
      ([], Just _) -> Nothing
      (xs, Just expr) ->
        Just ("(" ++ int ":" xs ++ ":" ++ show expr ++ ")")
    int sep xs = intercalate sep (map show xs)
    doNat expr = case expr of
      Con "Zero" args -> assert (null args) (0, Nothing)
      Con "Succ" args -> case args of
        [] -> (0, Just expr)
        [arg] -> case doNat arg of
          (n, e) -> (n+1, e)
        _ -> error $ "Invalid arguments for Succ: " ++ show args
      expr' -> (0, Just expr)
    doList expr = case expr of
      Con "Nil" args -> assert (null args) ([], Nothing)
      Con "Cons" args -> case args of
        [item, rest] -> case doList rest of
          (xs, e) -> (item:xs, e)
        _ -> ([], Just expr)
      expr' -> ([], Just expr)
    f <|> g = \expr -> case f expr of
      Nothing -> g expr
      Just s -> Just s

instance Show Pat where
  show (Pat tag vars) = unwords (tag:vars)

instance {-# OVERLAPPING #-} Show (Pat, Expr) where
  show (pat, alt) = show pat ++ " -> " ++ show alt
