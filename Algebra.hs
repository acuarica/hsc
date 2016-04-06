{-# LANGUAGE FlexibleInstances #-}

module Main where

newtype Fix f = Fx (f (Fix f))

showfm :: Fix Maybe -> String
showfm (Fx Nothing) = "Fx-Nothing"
showfm (Fx (Just a)) = "Fx-Just (" ++ showfm a ++ ")"

--showff :: Show (f a) => Fix f -> String
--showff (Fx a) = "Fx:" ++ show a

--newtype FM = FM (Fix Maybe)

showfl :: Fix [] -> String
showfl (Fx []) = "Fx-[]"
showfl (Fx (x:xs)) = "Fx-(" ++ showfl x ++ ":" ++ showfl (Fx xs) ++ ")"  

class ShowF f where
  showF :: Show a => f a -> String

instance ShowF f => Show (f (Fix f)) where
  show = showF

instance ShowF f => Show (Fix f) where
  show (Fx a) = "Fx" ++ show a

--instance ShowF (Maybe) where
 -- showF = show 
--instance Show

--instance Show (Fix Maybe) where
  --show = showfm

--instance Show (Fix []) where
  --show = showfl

a, b, c :: Fix Maybe
a = Fx Nothing 
b = Fx (Just (Fx Nothing))
c = Fx (Just (Fx (Just (Fx Nothing))))

d, e, f :: Fix []
d = Fx []
e = Fx [Fx [], Fx [], Fx []]
f = Fx [Fx [], Fx [Fx []], Fx []]
g = Fx [Fx [Fx [Fx [Fx [Fx []]]]]]

main :: IO ()
main = do
  putStrLn $ show a
  putStrLn $ showfm b
  putStrLn $ showfm c
  putStrLn $ showfl d
  putStrLn $ showfl e
  putStrLn $ showfl f
  putStrLn $ showfl g
  return ()

