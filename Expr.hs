{-# LANGUAGE FlexibleInstances #-}

{-|
  The Expr module defines the type Expr, the core type of the language.
  It also contains functions to easily manipulate Expr expressions.
-}
module Expr (
  Expr(Var, Con, Lam, App, Let, Case), Var, Tag, Binding, Alt, Pat(Pat),
  Subst,
  con, app, appVars, let1, isVar, isEmptyCon, bindings,
  subst, substAlts, lookupAlt, freeVars, alpha,
  true, false, zero, suc, nil, cons, bool, nat, list
) where

import Data.Maybe (fromMaybe)
import Data.List (nub, delete, (\\), union, intercalate)
import Control.Arrow (second)
import Control.Monad.State (State, state, runState, evalState)

--type Expr = ExprT Var Tag
--type Binding = (Var, Expr)
{-|
  Represents identifier variable.
-}
type Var = String

{-|
  Represents constructor name.
  Also called tag to be matched in case expressions.
-}
type Tag = String

{-|
  The expression type.
-}
data Expr v t
  = Var  v
  | Con  t [Expr v t]
  | Lam  v (Expr v t)
  | App  (Expr v t) (Expr v t)
  | Let  [Binding v t] (Expr v t)
  | Case (Expr v t) [Alt v t]
  deriving Eq

{-|
  A binding maps a variable to an expression.
-}
type Binding v t = (v, Expr v t)

{-|
  Represents an alternative within a case expression.
-}
type Alt v t = (Pat v t, Expr v t)

{-|
  Case patterns against tag.
-}
data Pat v t = Pat t [v] deriving Eq

{-|
  A substitution is a variable to be replaced with an expression.
-}
type Subst v t = Binding v t

{-|
  Creates an @Expr@ constructor with the given tag.
-}
con :: t -> Expr v t
con tag = Con tag []

{-|
  Applies the list of args to the given expr.
-}
app :: Expr v t -> [Expr v t] -> Expr v t
app expr args = case args of
  [] -> expr
  arg:args' -> app (App expr arg) args'

{-|
  Application of variable names to an expression.
-}
appVars :: Expr v t -> [v] -> Expr v t
appVars expr = app expr . map Var

{-|
  Creates a let expression with only one binding.
-}
let1 :: v -> Expr v t -> Expr v t -> Expr v t
let1 var valexpr = Let [(var, valexpr)]

{-|
  Returns True if expr is a variable.
  False otherwise.
-}
isVar :: Expr v t -> Bool
isVar (Var _) = True
isVar _ = False

{-|
  Returns True if expr is a constructor with no arguments.
  False otherwise.
-}
isEmptyCon :: Expr v t -> Bool
isEmptyCon (Con _ []) = True
isEmptyCon _ = False

{-|
  Retrives the list of variables from a list of bindings.
-}
vars :: [Binding v t] -> [v]
vars = fst . unzip

{-|
  Retrives the list of binded expressions from a list of bindings.
-}
bindings :: [Binding v t] -> [Expr v t]
bindings = snd . unzip

{-|
  Variable substitution.
  It substitutes var in bodyexpr only if var is a free variable.
  It does not substitute bound variables.
-}
subst :: Eq v => Subst v t -> Expr v t -> Expr v t
subst (var, valexpr) bodyexpr = case bodyexpr of
  Var var' ->
    if var' == var
      then valexpr
      else Var var'
  Con tag args -> Con tag (map goSubst args)
  Lam var' lamexpr' ->
    if var' == var
      then Lam var' lamexpr'
      else Lam var' (goSubst lamexpr')
  App funexpr' valexpr' -> App (goSubst funexpr') (goSubst valexpr')
  Let binds inexpr' ->
    if var `elem` vars binds
      then Let binds inexpr'
      else Let (map (second goSubst) binds) (goSubst inexpr')
  Case scexpr' alts -> Case (goSubst scexpr') (map goSubstAlt alts)
  where
    goSubst = subst (var, valexpr)
    goSubstAlt (Pat tag vars, altexpr) =
      (Pat tag vars, if var `elem` vars then altexpr else goSubst altexpr)

{-|
  Subtitutes a list of bindings in bodyexpr.
-}
substAlts :: Eq v => [Subst v t] -> Expr v t -> Expr v t
substAlts bindings bodyexpr = foldl (flip subst) bodyexpr bindings

{-|
  Lookup the alternative according to the constructor tag.
-}
lookupAlt :: (Eq t, Show t) => t -> [Alt v t] -> Alt v t
lookupAlt tag ((Pat pattag patvars, expr):alts) = if pattag == tag
  then (Pat pattag patvars, expr)
  else lookupAlt tag alts
lookupAlt tag [] = error $ "lookupAlt: " ++ show tag

{-|
  Free variables of an expression.
-}
freeVars :: Eq v => Expr v t -> [v]
freeVars expr = case expr of
  Var var -> [var]
  Con _ args -> nub (concatMap freeVars args)
  Lam var lamexpr -> delete var (freeVars lamexpr)
  App funexpr valexpr -> freeVars funexpr `union` freeVars valexpr
  Let binds inexpr ->
    (freeVars inexpr `union` concatMap freeVars (bindings binds)) \\
      vars binds
  Case scexpr alts -> nub (freeVars scexpr ++
    concatMap (\(Pat p vars, e) -> freeVars e \\ vars) alts)
--
-- class Monad m => VarGen m where
--   next :: m a

data VarGenStr t a = VarGenStr a (Expr Var t) --deriving (Functor)

get (VarGenStr _ e) = e

instance Functor (VarGenStr t) where
  fmap f (VarGenStr a expr) = VarGenStr (f a) expr

instance Applicative (VarGenStr t) where
  pure a = error ""
  f <*> a = error ""

  --fmap f (VarGenStr a expr) = VarGenStr (f a) expr

--  nextVar = (++) "$b_" . show
instance Monad (VarGenStr t) where
  return = pure
  a >>= f = error ""
--
-- instance VarGen (VarGenStr t) where
--   next = error ""

type VG a = State Int a

next :: VG String
next = state $ \ n -> ("$b_" ++ show n, n + 1)

{-|
  Alpha renaming of bound variables.
  This property comes handy when evaluating to Normal Form,
  since it avoids name capture.
  Forward usage:
    let a=c in let b=c in let c=X in a
-}
--a :: Expr Var t -> Expr Var t
alpha e = evalState (alpha' e) 0


alpha' :: Expr Var t -> VG (Expr Var t)
alpha' expr = case expr of
  Var var -> return $ Var var
  Con tag args -> return $ Con tag args
  Lam var lamexpr ->
    do
      newvar <- next
      lamexpr' <- alpha' (subst (var, Var newvar) lamexpr)
      return $ Lam newvar lamexpr'
  App funexpr valexpr ->
    do
      funexpr' <- alpha' funexpr
      valexpr' <- alpha' valexpr
      return $ App funexpr' valexpr'
  Let binds inexpr ->
    do
      let (vs, bs) = unzip binds
      --let next' = next + length binds - 1
      -- let vs' = map nextVar [next .. next']
      -- let ss = zip vs vs'
      -- let (n', bs') = doLet (next+length binds) ss (zip ss bs)
      -- let (next'', inexpr') = alphaSubst ss n' inexpr
      inexpr' <- alpha' inexpr
      return $ Let binds inexpr'
  --Case scexpr alts ->
  _ -> return expr

--
-- alpha' :: Expr Var t -> Expr Var t
-- alpha' = snd . doAlpha 0
--   where
--     doAlpha next expr = case expr of
--       Var var -> (next, Var var)
--       Con tag args -> (next, Con tag args)
--       Lam var lamexpr ->
--         let var' = nextVar next in
--         let (n', lamexpr') = alphaSubst [(var, var')]
                     --(next+1) lamexpr in
--         (n', Lam var' lamexpr')
--       App funexpr valexpr ->
--         let (next', funexpr') = doAlpha next funexpr in
--         let (next'', valexpr') = doAlpha next' valexpr in
--         (next'', App funexpr' valexpr')
--       Let binds inexpr ->
--         let (vs, bs) = unzip binds in
--         let next' = next + length binds - 1 in
--         let vs' = map nextVar [next .. next'] in
--         let ss = zip vs vs' in
--         let (n', bs') = doLet (next+length binds) ss (zip ss bs) in
--         let (next'', inexpr') = alphaSubst ss n' inexpr in
--         (next'', Let bs' inexpr')
--       Case scexpr alts ->
--         let (n', sc') = doAlpha next scexpr in
--         let (n'', as'') = doLet' n' alts in
--         (n'', Case sc' as'')
--     doLet' n [] = (n, [])
--     doLet' n ((p,e):as) =
--       let (next', e') = doAlpha n e in
--       let (n', as') = doLet' next' as in
--       (n', (p, e'):as')
--     doLet n _ [] = (n, [])
--     doLet n ss (((v,v'),e):bs) =
--       let (next', e') = alphaSubst ss n e in
--       let (n', bs') = doLet next' ss bs in
--       (n', (v', e'):bs')
--     alphaSubst ss n = doAlpha n . substAlts (map (second Var) ss)
--     nextVar = (++) "$b_" . show

{-|
  Some common used expressions for easy write of expressions.
  These expressions are pretty printed accordingly.
-}
true, false, zero, suc, nil, cons :: Expr v Tag
true = con "True"
false = con "False"
zero = con "Zero"
suc = con "Succ"
nil = con "Nil"
cons = con "Cons"

{-|
  Converts a Haskell Bool to an Expr.
  The resulting Expr uses the constructor true and false.
-}
bool :: Bool -> Expr v Tag
bool b = if b then true else false

{-|
  Converts a Haskell Int to an Expr.
  The resulting Expr uses the constructor zero and suc.
-}
nat :: Int -> Expr v Tag
nat n = if n > 0 then App suc (nat (n - 1)) else zero

{-|
  Given a way to construct expressions from a type,
  and a list of that type, returns an Expr representing that list.
  The resulting Expr uses the constructors nil and cons.
-}
list :: (a -> Expr v Tag) -> [a] -> Expr v Tag
list f xs = case xs of
  [] -> nil
  (x':xs') -> app cons [f x', list f xs']

instance (Show v, Show t) => Show (Expr v t) where
  show = show' False
    where
    show' par expr = case expr of
      Var var ->
        show var
      Con tag args -> --fromMaybe (
        if null args
          then show tag
          else "con con args" --paren par
      -- (show tag ++ " " ++ unwords (map (show' True) args)))
      --     ((prettyNat <|> prettyList) expr)

      Lam var expr ->
        "{" ++ show var ++ "->" ++ show expr ++ "}"
      Let binds inexpr ->
        paren par ("let " ++
        unwords (map (\(v, e)->show v ++ "=" ++ show' True e) binds) ++
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
      Con "Zero" args -> case args of
        [] -> (0, Nothing)
        _ -> error $ "Invalid arguments for Zero: " ++ showArgs args
      Con "Succ" args -> case args of
        [] -> (0, Just expr)
        [arg] -> case doNat arg of
          (n, e) -> (n+1, e)
        _ -> error $ "Invalid arguments for Succ: " ++ showArgs args
      expr' -> (0, Just expr)
    doList expr = case expr of
      Con "Nil" args -> case args of
        [] -> ([], Nothing)
        _ -> error $ "Invalid arguments for Nil: " ++ showArgs args
      Con "Cons" args -> case args of
        [item, rest] -> case doList rest of
          (xs, e) -> (item:xs, e)
        _ -> ([], Just expr)
      expr' -> ([], Just expr)
    f <|> g = \expr -> case f expr of
      Nothing -> g expr
      Just s -> Just s
    showArgs args = unwords (map show args)

instance {-# OVERLAPPING #-} (Show v, Show t) => Show (Binding v t) where
  show (var, expr) = show var ++ "=" ++ show expr

instance (Show v, Show t) => Show (Pat v t) where
  show (Pat tag vars) = show tag ++ " " ++ unwords (map show vars)

instance {-# OVERLAPPING #-} (Show v, Show t) => Show (Alt v t) where
  show (pat, alt) = show pat ++ " -> " ++ show alt
