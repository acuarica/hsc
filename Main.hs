
module Main where

import System.Exit (exitFailure)
import System.Environment (getArgs)
import System.FilePath (takeExtension)
import Language.Haskell.Exts (parseFileContents, fromParseResult)
import Data.List (intercalate)

import Expr (Expr(Var), Var)
import Parser (parseExpr)
import Eval (Conf, eval, emptyEnv, newConf, toExpr, step)
import Splitter (Node, Label)
import Supercompiler (Hist, supercompile, runMemo)
import HSE (fromHSE)

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

dotFromHist :: [(Var, Label, Var)] -> String
dotFromHist [] = ""
dotFromHist ((parentVar, label, var):es) =
  "\t\"" ++ parentVar ++ "\" -> \"" ++ var ++ "\"[label=\""++show label++"\"]\n" ++
  dotFromHist es

dotFromHist2 :: [(Var, [Var], Node, Conf)] -> String
dotFromHist2 [] = ""
dotFromHist2 ((var, fvs, node, (env, stack, expr)):vs) =
  "\t\"" ++ var ++       "\"[label=\"" ++ show expr ++ "\"]\n" ++
  dotFromHist2 vs

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
      putStrLn $ "[Supercompiling " ++ fileName ++ " ...]"
      fileText <- readFile fileName
      let expr = filterByExt ext fileText
      print expr
      print $ runMemo expr

      let sexpr = supercompile expr
      print sexpr
      --print $ (dot . newConf emptyEnv) expr
      let res = dot' $ (dot . newConf emptyEnv) expr
      --putStrLn res
      let dotsexpr = dot' $ (dot . newConf emptyEnv) sexpr
      let (_, ((es, vs), prom)) = runMemo expr
      let dotmm = wrapDot $ dotFromHist es ++ dotFromHist2 vs
      writeFile (fromFileName fileName "dot") dotmm --dotsexpr
      return ()
