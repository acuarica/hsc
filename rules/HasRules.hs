
module Main where

import System.Exit --(exitFailure)
import System.Environment (getArgs)
import System.FilePath
import System.Directory
import Language.Haskell.Exts -- (parseFileContents, fromParseResult)
import Data.List
import Control.Monad


hasRules :: Module -> Bool
hasRules (Module _ _ _ _ _ _ decls) = any hasRules' decls
  where
    hasRules' RulePragmaDecl{} = True
    hasRules' _ = False

fileHasRules :: ParseResult Module -> Bool
fileHasRules (ParseOk hse) = hasRules hse
fileHasRules _ = False

usage :: String
usage = "Usage: hsc <haskell file.hs>"

fromFileName :: FilePath -> String -> FilePath
fromFileName fileName ext = fileName ++ "." ++ ext

main :: IO ()
main = do
  args <- getArgs
  --let hackage = head args
  let fileName = head args
  fileText <- readFile fileName
  let hse = fromParseResult (parseFileContents fileText)
  --let res = hasRules hse
  --putStrLn $ fileName ++ ": " ++ show res
  --when res $ putStrLn $ fileName \\ hackage
  --when res $ copyFile fileName ("./rules" ++ (fileName \\ hackage))
  if hasRules hse then exitSuccess else exitFailure
  --return ()
