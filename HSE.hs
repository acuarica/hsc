
module HSE where

import Language.Haskell.Exts hiding (Pat,Var,Let,App,Case,Con,app,name)
import qualified Language.Haskell.Exts as H

import Expr (Expr(Var, Con, Lam, App, Case), Var, Pat(Pat), con, let1, zero)

fromHSE :: Show l => Expr -> Module l -> Expr
fromHSE rootExpr (Module _ _ _ _ decls) =
  foldr foldExpr rootExpr decls
  where foldExpr decl expr = case fromDecl decl of
          Nothing -> expr
          Just (v, d) -> let1 v d expr

fromDecl :: Show l => Decl l -> Maybe (Var, Expr)
fromDecl (PatBind _ (PVar _ f) (UnGuardedRhs _ x) (Just (BDecls _ []))) =
  Just (fromName f, fromExp x)
fromDecl (PatBind _ (PVar _ f) (UnGuardedRhs _ x) Nothing) =
  Just (fromName f, fromExp x)
fromDecl (FunBind l [Match _ f vars (UnGuardedRhs _ x) (Just (BDecls _ []))]) =
  Just (fromName f, fromExp $ Lambda l vars x)
fromDecl (FunBind l [Match _ f vars (UnGuardedRhs _ x) Nothing]) =
  Just (fromName f, fromExp $ Lambda l vars x)
fromDecl (FunBind l [Match _ f vars (UnGuardedRhs _ x) (Just (BDecls _ (d:ds)))]) =
  unhandle "fromExp:funbind" d
fromDecl (InstDecl l _overlap _rule (Just [InsDecl l' decl])) =
  fromDecl decl

fromDecl TypeSig{} = Nothing
fromDecl DataDecl{} = Nothing
fromDecl TypeDecl{} = Nothing
fromDecl RulePragmaDecl{} = Nothing
fromDecl decl = unhandle "fromDecl" decl

fromMatch (Match _ f [pat] (UnGuardedRhs _ x) Nothing) =
  (fromPat pat, fromExp x)
fromMatch (Match _ f [pat] (UnGuardedRhs _ x) (Just (BDecls _ []))) =
  (fromPat pat, fromExp x)
fromMatch m = unhandle "fromMatch" m

fromExp :: Show l => Exp l -> Expr
fromExp (Lambda _ [] x) = fromExp x
fromExp (Lambda l (PVar _ (Ident _ x):vars) bod) =
  Lam x $ fromExp $ Lambda l vars bod
fromExp (Lambda _l _pat body) =
  Lam "?" $ fromExp body
fromExp (H.App _ x y) = App (fromExp x) (fromExp y)
fromExp (H.Var _ (UnQual _ name)) = Var (fromName name)
fromExp (H.Con _ (UnQual _ name)) = con (fromName name)
fromExp (H.Con _ (Special _ (Cons _))) = con "Cons"
-- fromExp (LeftSection x (QVarOp y)) = fromExp $ H.App (H.Var y) x
fromExp (Paren _ x) = fromExp x
fromExp (H.Case _ sc alts) = Case (fromExp sc) (map fromAlt alts)
fromExp (List _ []) = con "Nil"
fromExp (List l (x:xs)) =
  fromExp $ InfixApp l x (QConOp l (Special l (Cons l))) $ List l xs

--fromExp (List (x:xs)) =
    --fromExp $ InfixApp x (QConOp (Special Cons)) $ List []

fromExp (InfixApp _ x (QConOp _ (Special _ (Cons _))) y) =
  Con "Cons" [fromExp x, fromExp y]
fromExp (InfixApp l a (QVarOp _ b) c) =
  fromExp $ H.App l (H.App l (H.Var l b) a) c
fromExp (Lit _ (Int _ n _)) | n >= 0 = f n
  where f n = if n == 0 then zero else Con "Succ" [f (n-1)]
fromExp (Lit _ (Char _ c _)) = con $ "$C" ++ [c]
fromExp (Lit _ (String _ "" _)) = con "Nil"
fromExp (Lit l (String _ (x:xs) _)) =
  Con "Cons" [fromExp (Lit l (Char l x "?")),
    fromExp (Lit l (String l xs "?"))]

--fromExp (Lit x) = Con noname (prettyPrint x) []
-- fromExp x@(NegApp _) = Con noname (prettyPrint x) []
fromExp (If _l a b c) = fromExp $ H.Case _l a [f "True" b, f "False" c]
  where
    f con x = Alt _l (PApp _l (UnQual _l (Ident _l con)) [])
      (UnGuardedRhs _l x) (Just (BDecls _l []))
-- fromExp o@(H.Let (BDecls xs) x) =
   --Let noname ((f1,fromExp x):concatMap fromDecl xs) f1
--     where f1:_ = freshNames o
-- fromExp o@(Tuple _ xs) =
--   Let noname
--     ((f1, Con noname (fromTuple xs) (take (length xs) fs)) :
--        zipWith (\f x -> (f,fromExp x)) fs xs) f1
--     where f1:fs = freshNames o
-- fromExp (H.Con (Special (TupleCon _ n))) =
--   Con noname (fromTuple $ replicate n ()) []
-- fromExp e@(Do stmts) =
--   error ("fromExp Do: " ++ prettyPrint e ++ "\n --or--\n" ++ show e)
fromExp (Tuple _l Boxed exps) = Con ("Tuple" ++ show (length exps)) $ map fromExp exps
fromExp e = unhandle "fromExp" e

fromName :: H.Name l -> Var
fromName (Ident _ var) = var
fromName (Symbol _ var) = var

fromAlt :: Show l => Alt l -> (Pat, Expr)
fromAlt (Alt _ pat (UnGuardedRhs _ bod) (Just (BDecls _ []))) =
  (fromPat pat, fromExp bod)
fromAlt (Alt _ pat (UnGuardedRhs _ bod) Nothing) =
  (fromPat pat, fromExp bod)
--fromAlt x = error $ "Unhandled fromAlt: " ++ show x

fromPat :: Show l => H.Pat l -> Pat
fromPat (PParen _ x) = fromPat x
fromPat (PList _ []) = Pat "Nil" []
fromPat (PApp _ (Special _ (Cons _ )) xs) = Pat "Cons" $ map fromPatVar xs
fromPat (PInfixApp l a b c) = fromPat $ PApp l b [a,c]
fromPat (PApp _ (UnQual _ c) xs) = Pat (fromName c) $ map fromPatVar xs
fromPat (PTuple _ _ xs) = Pat (fromTuple xs) $ map fromPatVar xs
fromPat (PApp _ (Special _ (TupleCon _ _ n)) xs) = Pat (fromTuple xs) $
    map fromPatVar xs
fromPat (PWildCard _) = Pat "_wild" []
fromPat x = unhandle "fromPat" x

fromTuple xs = "(" ++ replicate (length xs - 1) ',' ++ ")"

fromPatVar :: Show l => H.Pat l -> Var
fromPatVar (PVar _ x) = fromName x
fromPatVar (PWildCard _) = "$__new__"
fromPatVar x = unhandle "fromPatVar" x

unhandle :: (Show a, Pretty a) => String -> a -> t
unhandle w e = error $ "Unhandled HSE." ++ w ++ ": \n" ++
  show e ++ "\n -- or -- \n" ++ prettyPrint e
