
--fileText <- readFile "Setup.hs"
--print fileText
--print $ fromParseResult (parseFileContents fileText)
--print $ aform $ parseExpr "f (g (h x))"
--print $ aform $ parseExpr "f (g x) (g (h x)) (h y)"
print $ aform $ parseExpr "f (g x) (g (h x)) (h y)"
--putStrLn "To implement ..." >> exitFailure
