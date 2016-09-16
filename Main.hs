
module Main where

import System.Exit (exitFailure)
import System.Environment (getArgs)
import System.FilePath (takeExtension)
import Language.Haskell.Exts (parseFileContents, fromParseResult)
import Data.List (intercalate)
import Text.Printf (printf)

import Expr --(Expr(Var, Let), Var)
import Parser (parseExpr)
import Eval (Conf, eval, emptyEnv, newConf, toExpr, step)
import Splitter --(Node, Label)
import Supercompiler -- (Hist, supercompile, runMemo)
import HSE (fromHSE)

usage :: String
usage = "Usage: hsc <haskell-file.hs | expr-file.expr>"

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

dotFromHist :: [HistEdge] -> String
dotFromHist [] = ""
dotFromHist ((parentVar, label, var, fv, conf):es) =
  printf "\t\"%s\":\"%s\" -> \"%s\" [label=\"%s\"]\n"
    parentVar (show label) var (show label) ++
  --"\t\"" ++ parentVar ++ "\" -> \"" ++ var ++ "\"[label=\""++show label++"\"]\n" ++
  dotFromHist es

dotFromHist2 :: Var -> [HistNode] -> String
dotFromHist2 var0 [] = ""
dotFromHist2 var0 ((var, fvs, node, (env, stack, expr), sps):vs) =
  printf "\t\"%s\" [shape=record,label=\"{{<p0>%s|%s}|%s%s}\"%s]\n"
    var var (unwords fvs) (pprint node) ports style ++
  --"\t\"" ++ var ++       "\"[label=\"" ++ show expr ++ "\"]\n" ++
  dotFromHist2 var0 vs
  where
    style = if var == var0 then ",style=\"rounded,bold\"" else ""
    pprint VarNode = show expr
    pprint ArgNode = show expr ++ show stack
    pprint ConNode = let (Con t vs) = expr in t
    pprint CaseNode = "case " ++ show expr ++ " of"--let (Con t vs) = expr in t
    port = printf "<%s>%s"
    ports = case node of
      VarNode -> ""
      ArgNode -> ""
      ConNode -> "|{" ++ let (Con tag vs) = expr in intercalate "|" (zipWith (\v i -> port (tag ++"_"++show i) (show v)) vs [1..length vs] ) ++"}"
      CaseNode -> "|{" ++ intercalate "|" (map (\l-> port (show l) (show l)) (fst (unzip sps))) ++ "}"

  -- \\trankdir=LR\n\
wrapDot :: String -> String
wrapDot = printf
  "digraph G {\n\
  \\tnode [style=rounded, fontname=\"Monaco\", fontsize=12]\n\
  \\tedge [fontname=\"Monaco\", fontsize=12]\n\
  \%s\
  \}\n"

filterByExt :: String -> String -> Expr
filterByExt ext fileText = case lookup ext filters of
  Nothing -> error "Extension not found"
  Just f -> f fileText
  where
    filters = [
      (".expr", parseExpr),
      (".hs", fromHSE (Var "root") . fromParseResult . parseFileContents)
      ]

pprint :: Expr -> String
pprint (Let var valexpr inexpr) =
  "let " ++ var ++ "=" ++ show valexpr ++ "" ++ " in \n" ++ pprint inexpr
pprint expr = show expr

writeFileWithLog :: FilePath -> String -> IO ()
writeFileWithLog fileName content =
  do
    putStrLn $ "[Writing " ++ fileName ++ "]"
    writeFile fileName content
    return ()

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
      putStrLn $ "[Supercompiling " ++ fileName ++ "]"
      fileText <- readFile fileName
      let expr = filterByExt ext fileText
      let (sexpr, rm@((var0, expr0), ((es, vs), prom))) = supercompileWithMemo expr
      let dotmm = wrapDot $ dotFromHist es ++ dotFromHist2 var0 (reverse vs)

      writeFileWithLog (fromFileName fileName "hist") $ show rm
      writeFileWithLog (fromFileName fileName "sexpr") $ pprint sexpr
      writeFileWithLog (fromFileName fileName "dot") dotmm

      --let res = dot' $ (dot . newConf emptyEnv) expr
      --let dotsexpr = dot' $ (dot . newConf emptyEnv) sexpr

      return ()
