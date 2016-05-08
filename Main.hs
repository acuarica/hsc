
module Main where

import System.Exit

import Expr
import Parser
import Eval
import Language.Haskell.Exts
import HSE

one :: a
one = one

a = b
b = a



main :: IO ()
main = do
  --fileText <- readFile "Setup.hs"
  --print fileText
  --print $ fromParseResult (parseFileContents fileText)
  --print $ aform $ parseExpr "f (g (h x))"
  --print $ aform $ parseExpr "f (g x) (g (h x)) (h y)"
  print $ aform $ parseExpr "f (g x) (g (h x)) (h y)"
  --putStrLn "To implement ..." >> exitFailure
