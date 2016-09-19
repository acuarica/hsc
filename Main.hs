
module Main where

import Data.List (intercalate)
import Data.String.Utils (lstrip, startswith)
import System.Exit (exitFailure)
import System.Environment (getArgs)
import System.FilePath (takeExtension)
import Text.Printf (printf)
import Language.Haskell.Exts (parseFileContents, fromParseResult)

import Expr --(Expr(Var, Let), Var)
import Parser (parseExpr)
import Eval (Conf, eval, emptyEnv, newConf, toExpr, step)
import Splitter --(Node, Label)
import Supercompiler -- (Hist, supercompile, runMemo)
import HSE (fromHSE)

usage :: String
usage = "Usage: hsc <haskell-file.hs | expr-file.expr>"

fromFileName :: FilePath -> String -> FilePath
fromFileName fileName ext = fileName ++ "." ++ ext

-- \\tgraph [ordering=out]\n\

makeDot :: Var -> Hist -> String
makeDot var0 (es, vs) = printf
  "digraph G {\n\
  \\tnode [shape=record, style=rounded, fontname=\"Monaco\", fontsize=12]\n\
  \\tedge [fontname=\"Monaco\", fontsize=12]\n\
  \\t\"%s\" [style=\"rounded, bold\"]\n\
  \%s\n\
  \%s\
  \}\n"
  var0 (foldr ((++) . dotEdge) "" es) (foldr ((++) . dotNode) "" vs)
  where
    dotEdge (parentVar, label, var, fv, conf) =
      printf "\t\"%s\":\"%s\" -> \"%s\"\n"
        parentVar (show label) var
    dotNode (var, fvs, node, (env, stack, expr), sps) =
      let port = printf "<%s>%s" in
      let (pnode, ports) = case node of
            VarNode -> (show expr, "")
            ArgNode -> (show expr ++ show stack, "")
            ConNode ->
              (let (Con t vs) = expr in t,
              "|{" ++
              let (Con tag vs) = expr in
              intercalate "|"
                (zipWith (\v i -> port (tag ++"_"++show i) (show v)) vs [1..length vs] ) ++"}")
            CaseNode -> ("case " ++ show expr ++ " of", "|{" ++ intercalate "|" (map (\l-> port (show l) (show l)) (fst (unzip sps))) ++ "}") in
      printf "\t\"%s\" [label=\"{{%s|%s}|%s%s}\"]\n"
        var var (unwords fvs) pnode ports

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
      content <- readFile fileName
      let noComment = not . startswith "--" . lstrip
      let exprText = (unlines . filter noComment . lines) content
      let expr = filterByExt ext exprText
      let (sexpr, rm@((var0, expr0), (hist, _))) = supercompileWithMemo expr

      writeFileWithLog (fromFileName fileName "hist") (show rm)
      writeFileWithLog (fromFileName fileName "sexpr") (pprint sexpr)
      writeFileWithLog (fromFileName fileName "dot") (makeDot var0 hist)

      print rm

      return ()
