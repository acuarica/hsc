
module Main where

import System.Exit (exitFailure)
import System.Environment (getArgs)
import System.FilePath
import Language.Haskell.Exts (parseFileContents, fromParseResult)
import Data.List

import Eval
import Supercompiler
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

fromFileName :: FilePath -> String -> FilePath
fromFileName fileName ext = fileName ++ "." ++ ext

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
      print $ expr
      print $ runMemo expr
      let sexpr = supercompile expr
      --print $ (dot . newConf emptyEnv) expr
      let res = dot' $ (dot . newConf emptyEnv) expr
      putStrLn res
      let dotsexpr = dot' $ (dot . newConf emptyEnv) sexpr
      writeFile (fromFileName fileName "dot") dotsexpr
      return ()
