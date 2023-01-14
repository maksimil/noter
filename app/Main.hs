module Main where

import           Noter.ParseDocument
import           Noter.PrintDocument
import           System.Environment  (getArgs)

gracefulFail :: String -> IO a
gracefulFail err = do
  putStrLn $ "\x1b[31mError:\x1b[0m "++err
  fail err

handler :: SomeException -> IO a
handler ex = gracefulFail $ displayException ex

gracefulHandle :: IO a -> IO a
gracefulHandle = handle handler

newtype Args = Args FilePath

parseArgs :: [String] -> IO Args
parseArgs [fileName] = pure $ Args fileName
parseArgs _          =  gracefulFail "USAGE: noter [BOOKFILE]"

main :: IO ()
main = do
  Args fileName <- parseArgs =<< getArgs
  document <- gracefulHandle (readDocument <$> readFile fileName)
  putStr $ documentString defaultDocumentStyle document
