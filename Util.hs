
module Util (doTests) where

import System.Exit
import Test.HUnit

doTests :: (Eq b, Show b) => (a -> (b, b)) -> [a] -> IO ()
doTests test tests = do
  counts <- runTestTT (TestList (map (doTest . test) tests))
  -- exitWith (if errors counts > 0 || failures counts > 0
  --   then ExitFailure (errors counts + failures counts)
  --   else ExitSuccess)
  return ()
  where doTest (act, exp) = TestCase (assertEqual "" exp act)

-- doPrint :: Show a => (String, a, a) -> String
-- doPrint (msg, expexpr, actexpr) =
--   msg ++ " ~~> " ++ show actexpr
--
-- doPrints :: (a -> String) -> [a] -> IO ()
-- doPrints f = mapM_ (putStrLn . f)
