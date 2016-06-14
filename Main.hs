
module Main where

import System.Exit (exitFailure)
import System.Environment (getArgs)
import System.FilePath
import Language.Haskell.Exts (parseFileContents, fromParseResult)
import Data.List

import Expr
import Parser
import Eval
import Supercompiler
import HSE

usage :: String
usage = "Usage: hsc <haskell file.hs | expr file.expr>"

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

dotFromHist :: Hist -> String
dotFromHist [] = ""
dotFromHist ((parentVar, var, fvs, (env, stack, expr)):hist) = 
  "\t\"" ++ var ++       "\"[label=\"" ++ show expr ++ "\"]\n" ++
  "\t\"" ++ parentVar ++ "\" -> \"" ++ var ++ "\"[label=\"hola\"]\n" ++
  dotFromHist hist

wrapDot :: String -> String
wrapDot gr = "digraph G {\n" ++ gr ++ "}\n"

filterByExt :: String -> String -> Expr
filterByExt ext fileText = case lookup ext filters of
  Nothing -> error "Extension not found"
  Just f -> f fileText
  where
    filters = [
      (".expr", parseExpr), 
      (".hs", fromHSE (Var "root") . fromParseResult . parseFileContents)
      ]

main :: IO ()
main = do
  args <- getArgs
  if null args
    then do
      putStrLn usage
      exitFailure
    else do
      let fileName = head args
      let ext = takeExtension fileName
      putStrLn $ "Processing " ++ fileName ++ " ..."
      fileText <- readFile fileName
      let expr = filterByExt ext fileText
      print $ expr
      print $ runMemo expr
      
      let sexpr = supercompile expr
      --print $ (dot . newConf emptyEnv) expr
      let res = dot' $ (dot . newConf emptyEnv) expr
      putStrLn res
      let dotsexpr = dot' $ (dot . newConf emptyEnv) sexpr
      let (_, (_, hist, _)) = runMemo expr
      let dotmm = wrapDot $ dotFromHist hist 
      writeFile (fromFileName fileName "dot") dotmm --dotsexpr
      return ()
