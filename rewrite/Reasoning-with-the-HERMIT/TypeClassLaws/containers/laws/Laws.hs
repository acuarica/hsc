module Main where

import Control.Applicative (pure, (<*>), Applicative)
import Control.Monad (liftM)
import Data.Map
import Data.Monoid ((<>), mempty, Monoid(..))

main :: IO ()
main = return ()

-- functor
{-# RULES "fmap-id"      [~]             fmap id = id #-}
{-# RULES "fmap-distrib" [~] forall g h. fmap (g.h) = fmap g . fmap h #-}

-- monoid
{-# RULES "mempty-left"  [~] forall x. mempty <> x = x #-}
{-# RULES "mempty-right" [~] forall x. x <> mempty = x #-}
{-# RULES "mappend-assoc" [~] forall x y z. (x <> y) <> z = x <> (y <> z) #-}
