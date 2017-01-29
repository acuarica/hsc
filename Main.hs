
module Main (main) where

import Data.Tree

import System.IO (hPutStrLn, stderr)

import Data.Maybe (isJust)

import Data.List (intercalate, isPrefixOf)
import Data.Char (isSpace)
import System.Exit (exitFailure)
import System.Environment (getArgs)
import System.FilePath (takeExtension)
import Text.Printf (printf)
import Language.Haskell.Exts (parseFileContents, fromParseResult)

import Expr -- (Expr(Var, Con, Let), Var)

import Eval -- (Conf, eval, step)

import Parser (parseExpr)
import Supercompiler -- (Hist, Node(VarNode, ArgNode, ConNode, CaseNode), supercompileMemo)
import HSE (fromHSE)
import Match

usage :: String
usage = "Usage: hsc <haskell-file.hs | expr-file.expr>"

makeName :: FilePath -> String -> FilePath
makeName fileName ext = fileName ++ "." ++ ext

makeDot :: String -> Var -> Hist -> String
makeDot caption var0 (es, vs) = printf
  "digraph G {\n\
  \\tgraph [label=\"%s\", \n\
  \\t  labelloc=t, fontname=\"Monaco\", fontsize=12]\n\
  \\tnode [shape=record, style=rounded, \n\
  \\t  fontname=\"Monaco\", fontsize=12]\n\
  \\tedge [fontname=\"Monaco\", fontsize=12, dir=both, arrowtail=box]\n\
  \\t\"%s\" [style=\"rounded, bold\"]\n\
  \%s\n\
  \%s\
  \}\n"
  caption var0 (cc dotEdge es) (cc dotNode vs)
  where
    cc d = foldr ((++) . d) ""
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
                (zipWith (\v i ->
                  port (tag ++"_"++show i) (show v)) vs [1..length vs] )
                ++"}")
            CaseNode -> ("case " ++ show expr ++ " of", "|{" ++
              intercalate "|" (map (\l->
                port (show l) (show l)) (fst (unzip sps))) ++ "}") in
      printf "\t\"%s\" [label=\"{{%s|%s}|%s%s}\"]\n"
        var var (unwords fvs) pnode ports

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

cc :: (Node, [(Label, Conf)]) -> [Conf]
cc (_, cs) = snd $ unzip cs

split' :: Conf -> [Conf]
split' = cc . split

drive :: Conf -> Tree Conf
drive conf = case step conf of
  Nothing -> Node conf (map drive $ split' conf)
  Just conf' -> Node conf [drive conf']

-- expr (_, _, e) = e

uni :: Conf -> Conf -> Bool
uni (env, _, e) (env', _, e') = case e |~~| e' of
  Nothing -> False
  Just xs -> all (\(v,e)->isVar e && n v env && n (let Var v'=e in v') env') xs

n :: Var -> Env -> Bool
n v ls = v `notElem` (fst . unzip) ls

term :: Tree Conf -> Tree Conf
term t = term' [] t
  where term' xs (Node conf cs) = if any (uni conf) xs
          then Node conf []
          else Node conf (fmap (term' (conf:xs)) cs)

-- printTrace :: [Conf] -> IO ()
-- printTrace = foldr ((>>) . print) (return ())

depth :: Int -> Tree a -> Tree a
depth 1 (Node x _xs) = Node x []
depth n (Node x xs) = Node x (fmap (depth (n - 1)) xs)

dropEnv :: Conf -> (Stack, Expr)
dropEnv (_env, stack, expr) = (stack, expr)

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
      -- let (sexpr, rm@((v0, e0), (h, _))) = supercompileMemo expr

      -- writeFileLog (makeName fname "hist") (show rm)
      -- writeFileLog (makeName fname "sexpr") (pprint sexpr)
      -- writeFileLog (makeName fname "dot") (makeDot (caption expr) v0 h)

      -- putStrLn "-- Expression to supercompile"
      -- print expr
      -- putStrLn "-- Evaluated expression"
      -- print $ eval expr
      -- putStrLn "-- Supercompiled expression"
      -- putStrLn $ pprint sexpr

      -- putStrLn $ drawTree $ fmap (show . dropEnv) $ depth 20 $ trace $ newConf emptyEnv expr
      putStrLn $ drawTree $ fmap (show . dropEnv) $ term $ drive $ newConf emptyEnv expr

      return ()
