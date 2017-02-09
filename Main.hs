
module Main (main) where

import Control.Arrow (first, second)
import Data.List (isPrefixOf)
import Data.Char (isSpace)
import System.Exit (exitFailure)
import System.Environment (getArgs)
import System.FilePath (takeExtension)
import System.IO (hPutStrLn, stderr)
import Language.Haskell.Exts (parseFileContents, fromParseResult)

import Expr (Expr(Var, Let))
import Eval (dropEnv)
import Parser (parseExpr)
import Tree (draw)
import Supercompiler (ptree, residuate)
import HSE (fromHSE)

usage :: String
usage = "Usage: hsc <haskell-file.hs | expr-file.expr>"

makeName :: FilePath -> String -> FilePath
makeName fileName ext = fileName ++ "." ++ ext

-- makeDot :: String -> Var -> Hist -> String
-- makeDot caption var0 (es, vs) = printf
--   "digraph G {\n\
--   \\tgraph [label=\"%s\", \n\
--   \\t  labelloc=t, fontname=\"Monaco\", fontsize=12]\n\
--   \\tnode [shape=record, style=rounded, \n\
--   \\t  fontname=\"Monaco\", fontsize=12]\n\
--   \\tedge [fontname=\"Monaco\", fontsize=12, dir=both, arrowtail=box]\n\
--   \\t\"%s\" [style=\"rounded, bold\"]\n\
--   \%s\n\
--   \%s\
--   \}\n"
--   caption var0 (cc dotEdge es) (cc dotNode vs)
--   where
--     cc d = foldr ((++) . d) ""
--     dotEdge (parentVar, label, var, fv, conf) =
--       printf "\t\"%s\":\"%s\" -> \"%s\"\n"
--         parentVar (show label) var
--     dotNode (var, fvs, node, (env, stack, expr), sps) =
--       let port = printf "<%s>%s" in
--       let (pnode, ports) = case node of
--             VarNode -> (show expr, "")
--             ArgNode -> (show expr ++ show stack, "")
--             ConNode ->
--               (let (Con t vs) = expr in t,
--               "|{" ++
--               let (Con tag vs) = expr in
--               intercalate "|"
--                 (zipWith (\v i ->
--                   port (tag ++"_"++show i) (show v)) vs [1..length vs] )
--                 ++"}")
--             CaseNode -> ("case " ++ show expr ++ " of", "|{" ++
--               intercalate "|" (map (\l->
--                 port (show l) (show l)) (fst (unzip sps))) ++ "}") in
--       printf "\t\"%s\" [label=\"{{%s|%s}|%s%s}\"]\n"
--         var var (unwords fvs) pnode ports

parseHs :: String -> Expr
parseHs = fromHSE (Var "root") . fromParseResult . parseFileContents

filterByExt :: String -> String -> Expr
filterByExt ext fileText = case lookup ext filters of
  Nothing -> error "Extension not found"
  Just f -> f fileText
  where
    filters = [
      (".expr", parseExpr),
      (".hs", parseHs),
      (".core", parseHs)
      ]

pprint :: Expr -> String
pprint (Let binds inexpr) =
  "let \n" ++ unwords (map pplet binds) ++ "in \n" ++ pprint inexpr
  where pplet (var, valexpr) = "  " ++ var ++ "=" ++ show valexpr ++ "\n"
pprint expr = show expr

caption :: Expr -> String
caption (Let _ inexpr) = caption inexpr
caption expr = show expr

writeFileLog :: FilePath -> String -> IO ()
writeFileLog fileName content =
  do
    hPutStrLn stderr $ "{- Writing " ++ fileName ++ " -}"
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
      let fname = head args
      let ext = takeExtension fname
      putStrLn $ "{- Supercompiling " ++ fname ++ " -}"
      content <- readFile fname

      let noComment = not . isPrefixOf "--" . dropWhile isSpace
      let exprText = (unlines . filter noComment . lines) content
      let expr = filterByExt ext exprText

      -- let pt = ptree expr
      -- let dpt = draw $ (second . first) dropEnv <$> pt
      -- let sexpr = residuate pt

      -- writeFileLog (makeName fname "ptree") dpt
      -- writeFileLog (makeName fname "sexpr") (show sexpr)

      -- putStrLn dpt
      -- print sexpr

      putStrLn $ draw $ (second . first) dropEnv <$> ptree expr

      return ()
