module Main where

import           Noter.ParseDocument

main :: IO ()
main = do
  file <- readDocument <$> readFile "data"
  print file
  return ()
