
module Main where

newtype Fix f = Fx (f (Fix f))

class ShowF f where
  showF :: Show a => f a -> String

instance ShowF [] where
  showF = show

instance ShowF Maybe where
  showF = show

instance (ShowF f) => Show (Fix f) where
  show (Fx a) = "Fx# (" ++ showF a ++ ")"

a, b, c :: Fix Maybe
a = Fx Nothing 
b = Fx (Just (Fx Nothing))
c = Fx (Just (Fx (Just (Fx Nothing))))

d, e, f, g, h :: Fix []
d = Fx []
e = Fx [Fx [], Fx [], Fx []]
f = Fx [Fx [], Fx [Fx []], Fx []]
g = Fx [Fx [Fx [Fx [Fx [Fx []]]]]]
h = Fx [Fx [Fx [], Fx []], Fx[Fx [], Fx[]]]

main :: IO ()
main = do
  putStrLn $ show a
  putStrLn $ show b
  putStrLn $ show c
  putStrLn $ show d
  putStrLn $ show e
  putStrLn $ show f
  putStrLn $ show g
  print h
  return ()

