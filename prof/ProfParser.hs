
module Main (main) where

import Prof (prof)
import Parser (parseExpr)
import Expr

loopString :: Int -> String -> Expr
loopString n [] = Var (show n)
loopString n (x:xs) = loopString (n+if x == '.' then 1 else 0) xs

main :: IO ()
-- main = prof parseExpr
main = prof $ loopString 0
