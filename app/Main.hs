module Main where

import           Noter.ParseDocument
import           Noter.PrintDocument

main :: IO ()
main = do
  file <- readDocument <$> readFile "data"
  putStr $ documentString defaultDocumentStyle file
  return ()
