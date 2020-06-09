{-# LANGUAGE OverloadedStrings #-}
module HERMIT.Libraries.Century (lemmas) where

import Data.Monoid

import HERMIT.Lemma
import HERMIT.GHC hiding (($$), (<>))
import HERMIT.Kure
import HERMIT.Name

import HERMIT.Dictionary.Common
import HERMIT.Dictionary.Function
import HERMIT.Dictionary.GHC
import HERMIT.Dictionary.Reasoning
import HERMIT.Dictionary.Undefined

lemmas :: LemmaLibrary
lemmas = foldrFusionLemmaT <> filterSplitLemmaT

-------------------------------------------------

-- | foldr fusion
--
-- f :: A -> B
-- g :: C -> A -> A
-- h :: C -> B -> B
-- a :: A
-- b :: B
--
-- strict f    /\    f a = b    /\    forall x y. f (g x y) = h x (f y)
-----------------------------------------------------------------------
--           f . foldr g a = foldr h b
--
foldrFusionLemmaT :: LemmaLibrary
foldrFusionLemmaT = do
    foldrId <- findIdT "Data.List.foldr"
    contextonlyT $ \ c -> do
        [aTv, bTv, cTv] <- mapM newTyVar ["a","b","c"]

        let [aTy, bTy, cTy] = map mkTyVarTy [aTv, bTv, cTv]

        a <- newVarH "a" aTy
        b <- newVarH "b" bTy
        f <- newVarH "f" (aTy --> bTy)
        x <- newVarH "x" cTy
        y <- newVarH "y" aTy
        g <- newVarH "g" (cTy --> aTy --> aTy)
        h <- newVarH "h" (cTy --> bTy --> bTy)

        fUndef <- inContextM c (applyToUndefinedT $ toCE f)
        undef <- inContextM c (mkUndefinedValT bTy)
        fa <- f $$ a
        gxy <- g $$$ [x,y]
        fgxy <- f $$ gxy
        fy <- f $$ y
        hxfy <- h $$$ [toCE x, fy]
        foldrga <- foldrId $$$ [g,a]
        foldrhb <- foldrId $$$ [h,b]
        fafterfoldrga <- inContextM c $ buildCompositionT (toCE f) foldrga

        return $ newLemma "foldr-fusion" $
           mkForall [aTv, bTv, cTv, f, g, h, a, b] $
              ("foldr-fusion-antecedent", fUndef === undef /\ fa === b /\ mkForall [x,y] (fgxy === hxfy))
                  ==>
              fafterfoldrga === foldrhb

-------------------------------------------------

-- | filter split
--
-- forall p q.
--   (forall x.  q x = False  =>  p x = False)
-- =>
--   filter p = filter p . filter q
--

filterSplitLemmaT :: LemmaLibrary
filterSplitLemmaT = do
    filterId <- findIdT "filter" -- not Data.List.filter, which has no unfolding
    compId <- findIdT "."
    constT $ do
        a <- newTyVar "a"

        let aTy = mkTyVarTy a

        p <- newVarH "p" (aTy --> boolTy)
        q <- newVarH "q" (aTy --> boolTy)
        x <- newVarH "x" aTy

        qx <- q $$ x
        px <- p $$ x
        filterp <- filterId $$ p
        filterq <- filterId $$ q
        filterpcomp <- compId $$ filterp
        filterpq <- filterpcomp $$ filterq

        return $ newLemma "filter-split" $
          mkForall [a, p, q] $
             ("filter-split-antecedent", mkForall [x] (("qx-False", qx === falseDataConId) ==> px === falseDataConId))
                 ==>
             filterp === filterpq

-------------------------------------------------

newTyVar :: MonadUnique m => String -> m TyVar
newTyVar nm = newVarH nm liftedTypeKind

-------------------------------------------------
