

{-# OPTIONS_GHC -O -ddump-rule-firings #-}

import Control.Exception
import System.CPUTime
import Text.Printf

append :: [a] -> [a] -> [a]
append xs ys = case xs of
	[] -> ys
	(x:xxs) -> x:(append xxs ys)
{-# NOINLINE append #-}


app3 :: [a] -> [a] -> [a] -> [a]
app3 [] ys zs = append ys zs
app3 (x:xs) ys zs = x:app3 xs ys zs

{-# RULES
  "****APPEND/APP3****" forall xs ys zs.  append (append xs ys) zs = app3 xs ys zs
  #-}

mapl :: (a -> b) -> [a] -> [b]
mapl f [] = []
mapl f (x:xs) = f x : mapl f xs

l1 = [1..10000]
l2 = [1001..20000]
l3 = [2001..30000]

x1 = append [1,2,3] [1,2,3]


a3 xs ys zs = append (append xs ys) zs

apply xs ys zs n = case n of 
	0 -> 0
	_ -> length (a3 xs ys zs) + apply xs ys zs (n-1)

time :: IO t -> IO t
time a = do
    start <- getCPUTime
    v <- a
    end   <- getCPUTime
    let diff = (fromIntegral (end - start)) / (10^12)
    printf "Computation time: %0.3f sec\n" (diff :: Double)
    return v

appmap xs 0 = 0
appmap xs n = length (mapl (*3) (mapl (+2) xs) ) + appmap xs (n-1)

{-# RULES
--    "****MAP/APPMAP***" forall f g xs . mapl f (mapl g xs) = mapl (f.g) xs
  #-}

main = do
	putStrLn "hola"
	putStrLn $ show $ length x1
--	time $ apply l1 l2 l3 50000 `seq` return ()
	time $ appmap l1 20000 `seq` return ()

