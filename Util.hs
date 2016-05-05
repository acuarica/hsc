
module Util where

import System.Exit
import Test.HUnit

doTest :: (Show a, Eq b, Show b) => (a, b, b) -> Test
doTest (msg, exp, act) = TestCase (assertEqual (show msg) exp act)

doTests :: (a -> Test) -> [a] -> IO ()
doTests test tests = do
  counts <- runTestTT (TestList (map test tests))
  exitWith (if errors counts > 0 || failures counts > 0
    then ExitFailure (errors counts + failures counts)
    else ExitSuccess)

doPrints :: (a -> String) -> [a] -> IO ()
doPrints f = mapM_ (putStrLn . f)
