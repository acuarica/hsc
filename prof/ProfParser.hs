
module Main (main) where

import Prof (prof)
import Parser (parseExpr)

main :: IO ()
main = prof parseExpr
