module Main where

newtype Age = MkAge Int

main :: IO ()
main = return ()
  where
    -- This won't compile if the role annotations are incorrect:
    
