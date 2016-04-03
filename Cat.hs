
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  file <- readFile (args!!0) 
  putStrLn $ file
