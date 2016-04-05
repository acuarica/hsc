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

instance Show (Fix Maybe) where
  show = showfm

instance Show (Fix []) where
  show = showfl

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
  print a
  print b
  print c
  print d
  print e
  print f
  print g
  return ()

