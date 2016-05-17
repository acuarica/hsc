
module Util (doTests) where

import System.Exit (ExitCode(..), exitWith)
import Test.HUnit (Test(..), errors, failures, assertEqual, runTestTT)

-- | Run tests with the test function.
-- | Creates a wrapper TestCase from the tuple.
doTests :: (Eq b, Show b) => (a -> (b, b)) -> [a] -> IO ()
doTests test tests = do
  counts <- runTestTT (TestList (map (doTest . test) tests))
  exitWith (if errors counts > 0 || failures counts > 0
    then ExitFailure (errors counts + failures counts)
    else ExitSuccess)
  return ()
  where doTest (act, exp) = TestCase (assertEqual "" exp act)
