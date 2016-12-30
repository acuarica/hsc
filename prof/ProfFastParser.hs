
module Main(main) where

import Prof (prof)
import FastParser (parseExpr)

main :: IO ()
main = prof parseExpr
