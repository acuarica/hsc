
module Main where

import System.Exit (exitFailure)
import System.Environment (getArgs)
import Language.Haskell.Exts (parseFileContents, fromParseResult)

import HSE

usage :: String
usage = "Usage: hsc <haskell file.hs>"

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
      print $ fromHSE hse
      return ()
