module Main where

import           Control.Exception   (Exception (displayException),
                                      SomeException, handle)
import           GHC.IO.Handle       (BufferMode (NoBuffering), hSetBuffering,
                                      hSetEcho)
import           Noter.ParseDocument
import           Noter.UI            (ChooseChapterState (..), DrawState (draw),
                                      UIGlobalState (UIGlobalState),
                                      UIState (ChooseChapterState), react)
import           Noter.Utils         (if')
import           System.Environment  (getArgs)
import           System.IO           (hReady, stdin, stdout)

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

getKey :: IO String
getKey = reverse <$> getKey' ""
  where
    getKey' :: String -> IO String
    getKey' chars = do
      char <- getChar
      more <- hReady stdin
      if' more getKey' pure  (char:chars)

run :: UIState -> IO ()
run state = do
  putStr $ draw state

  key <- getKey
  run' $ react state key
    where
      run' :: Maybe UIState -> IO ()
      run' Nothing         = pure ()
      run' (Just newState) = run newState

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  hSetEcho stdin False

  Args fileName <- parseArgs =<< getArgs
  document <- gracefulHandle (readDocument <$> readFile fileName)
  -- putStr $ documentString defaultDocumentStyle document

  putStr "\x1b[?1049h\x1b[?25l\x1b[H"

  let initState = ChooseChapterState (ChooseChapter (UIGlobalState document) 0)

  run initState

  putStr "\x1b[2J\x1b[?1049l\x1b[?25h"
