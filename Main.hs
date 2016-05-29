
module Main where

import System.Exit (exitFailure)
import System.Environment (getArgs)
import Language.Haskell.Exts (parseFileContents, fromParseResult)
import Data.List

import Eval
import HSE

usage :: String
usage = "Usage: hsc <haskell file.hs>"

dot :: Conf -> [Conf]
dot conf = conf:(case step conf of
  Nothing -> []
  Just conf' -> dot conf')

dot' :: [Conf] -> String
dot' cs = "digraph G {" ++ tolist cs ++ "}"
  where
    tolist cs = intercalate "->" (map tostr cs)
    tostr conf = "\"" ++ show (toExpr conf) ++ "\""

main :: IO ()
main = do
  args <- getArgs
  if null args
    then do
      putStrLn usage
      exitFailure
    else do
      let fileName = head args
      putStrLn $ "Processing " ++ fileName
      fileText <- readFile fileName
      let hse = fromParseResult (parseFileContents fileText)
      let expr = fromHSE hse
      --print $ expr
      --print $ (dot . newConf emptyEnv) expr
      let res = dot' $ (dot . newConf emptyEnv) expr
      putStrLn $ res
      writeFile "hola.dot" res
      return ()
