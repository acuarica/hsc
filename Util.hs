
module Util (doTests) where

import System.Exit
import Test.HUnit

doTests :: (Eq b, Show b) => (a -> (String, b, b)) -> [a] -> IO ()
doTests test tests = do
  --doPrints doPrint ts
  counts <- runTestTT (TestList (map doTest ts))
  -- exitWith (if errors counts > 0 || failures counts > 0
  --   then ExitFailure (errors counts + failures counts)
  --   else ExitSuccess)
  return ()
  where ts = map test tests

doTest :: (Eq a, Show a) => (String, a, a) -> Test
doTest (msg, exp, act) = TestCase (assertEqual msg exp act)

doPrint :: Show a => (String, a, a) -> String
doPrint (msg, expexpr, actexpr) =
  msg ++ " ~~> " ++ show actexpr

doPrints :: (a -> String) -> [a] -> IO ()
doPrints f = mapM_ (putStrLn . f)
