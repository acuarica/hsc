

import Prelude
import Data
import System.IO

root :: IO ()
root = print . length . words =<< getContents
