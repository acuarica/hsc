
module HSE where

import Language.Haskell.Exts hiding (Pat,Var,Let,App,Case,Con,app,name)
import qualified Language.Haskell.Exts as H

import Expr

fromHSE :: Module -> Expr
fromHSE (Module _ _ _ _ _ _ decls) = foldr foldExpr (Var "root") decls
  where foldExpr decl expr = case fromDecl decl of
          Nothing -> expr
          Just (v, d) -> Let v d expr

fromDecl :: Decl -> Maybe (Var, Expr)
fromDecl (PatBind _ (PVar f) (UnGuardedRhs x) (Just (BDecls []))) =
  Just (fromName f, fromExp x)
fromDecl (PatBind _ (PVar f) (UnGuardedRhs x) Nothing) =
  Just (fromName f, fromExp x)
fromDecl (FunBind [Match _ f vars Nothing (UnGuardedRhs x) (Just (BDecls []))]) =
  Just (fromName f, fromExp $ Lambda sl vars x)
fromDecl (FunBind [Match _ f vars Nothing (UnGuardedRhs x) Nothing]) =
  Just (fromName f, fromExp $ Lambda sl vars x)

fromDecl d@(FunBind ms) = unhandle "fromDecl:test" d
--  Just ("$$", Lam "$scvar" (Case (Var "$scvar") (map fromMatch ms)))

fromDecl TypeSig{} = Nothing
fromDecl DataDecl{} = Nothing
fromDecl TypeDecl{} = Nothing
fromDecl decl = unhandle "fromDecl" decl

fromMatch (Match _ f [pat] Nothing (UnGuardedRhs x) Nothing) =
  (fromPat pat, fromExp x)
fromMatch (Match _ f [pat] Nothing (UnGuardedRhs x) (Just (BDecls []))) =
  (fromPat pat, fromExp x)
fromMatch m = unhandle "fromMatch" m

fromExp :: Exp -> Expr
fromExp (Lambda _ [] x) = fromExp x
fromExp (Lambda _ (PVar (Ident x):vars) bod) = Lam x $ fromExp $ Lambda sl vars bod
fromExp (H.App x y) = App (fromExp x) (fromExp y)
fromExp (H.Var (UnQual name)) = Var (fromName name)
fromExp (H.Con (UnQual name)) = con (fromName name)
fromExp (H.Con (Special Cons)) = con "Cons"
-- fromExp (LeftSection x (QVarOp y)) = fromExp $ H.App (H.Var y) x
fromExp (Paren x) = fromExp x
fromExp (H.Case sc alts) = Case (fromExp sc) (map fromAlt alts)
fromExp (List []) = con "Nil"
fromExp (List (x:xs)) = fromExp $ InfixApp x (QConOp (Special Cons)) $ List xs

--fromExp (List (x:xs)) = fromExp $ InfixApp x (QConOp (Special Cons)) $ List []

fromExp (InfixApp x (QConOp (Special Cons)) y) = Con "Cons" [fromExp x, fromExp y]
fromExp (InfixApp a (QVarOp b) c) = fromExp $ H.App (H.App (H.Var b) a) c
fromExp (Lit (Int n)) | n >= 0 = f n
  where f n = if n == 0 then zero else Con "Succ" [f (n-1)]
fromExp (Lit (Char c)) = con $ "$C" ++ [c]
fromExp (Lit (String "")) = con "Nil"
fromExp (Lit (String (x:xs))) =
  Con "Cons" [fromExp (Lit (Char x)), fromExp (Lit (String xs))]

--fromExp (Lit x) = Con noname (prettyPrint x) []
-- fromExp x@(NegApp _) = Con noname (prettyPrint x) []
-- fromExp (If a b c) = fromExp $ H.Case a [f "True" b, f "False" c]
--     where f con x = Alt sl (PApp (UnQual $ Ident con) []) (UnGuardedRhs x) (Just (BDecls []))
-- fromExp o@(H.Let (BDecls xs) x) = Let noname ((f1,fromExp x):concatMap fromDecl xs) f1
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
fromExp e = unhandle "fromExp" e

fromName :: H.Name -> Var
fromName (Ident var) = var
fromName (Symbol var) = var

fromAlt :: Alt -> (Pat, Expr)
fromAlt (Alt _ pat (UnGuardedRhs bod) (Just (BDecls []))) =
  (fromPat pat, fromExp bod)
fromAlt (Alt _ pat (UnGuardedRhs bod) Nothing) =
  (fromPat pat, fromExp bod)
fromAlt x = error $ "Unhandled fromAlt: " ++ show x

fromPat :: H.Pat -> Pat
fromPat (PParen x) = fromPat x
fromPat (PList []) = Pat "Nil" []
fromPat (PApp (Special Cons) xs) = Pat "Cons" $ map fromPatVar xs
fromPat (PInfixApp a b c) = fromPat $ PApp b [a,c]
fromPat (PApp (UnQual c) xs) = Pat (fromName c) $ map fromPatVar xs
fromPat (PTuple _ xs) = Pat (fromTuple xs) $ map fromPatVar xs
fromPat (PApp (Special (TupleCon _ n)) xs) = Pat (fromTuple xs) $
    map fromPatVar xs
fromPat PWildCard = Pat "_wild" []
fromPat x = unhandle "fromPat" x

fromTuple xs = "(" ++ replicate (length xs - 1) ',' ++ ")"

fromPatVar :: H.Pat -> Var
fromPatVar (PVar x) = fromName x
fromPatVar PWildCard = "$__new__"
fromPatVar x = unhandle "fromPatVar" x

sl :: SrcLoc
sl = SrcLoc "" 0 0

unhandle :: (Show a, Pretty a) => String -> a -> t
unhandle w e = error $ "Unhandled HSE." ++ w ++ ": \n" ++
  show e ++ "\n -- or -- \n" ++ prettyPrint e
