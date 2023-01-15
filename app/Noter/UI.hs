module Noter.UI where

import           Noter.ParseDocument (Contents, Document, Heading)
import           Noter.PrintDocument (joinList)
import           Noter.Utils         (if')

class DrawState a where
  draw :: a -> String

newtype UIGlobalState = UIGlobalState Document

type ChapterIdx = Int
type LineIdx = Int

data UIState = ChooseChapterState ChooseChapterState | ViewChapterState ViewChapterState

data ChooseChapterState = ChooseChapter UIGlobalState ChapterIdx
data ViewChapterState = ViewChapter UIGlobalState ChapterIdx LineIdx

instance DrawState ChooseChapterState where
  draw (ChooseChapter global chapterIdx)  = joinList "\n" $ formatHeading <$> zip [0..] document
    where
      (UIGlobalState document) = global

      formatHeading :: (Int, (Heading, Contents)) -> String
      formatHeading (idx, (heading, _)) = if' (idx==chapterIdx) chosenFormat unchosenFormat
        where
          chosenFormat = "\x1b[34m> "++heading++"\x1b[0m"
          unchosenFormat = "+ "++heading

instance DrawState ViewChapterState where
  draw _ = "viewchapter"

instance DrawState UIState where
  draw (ChooseChapterState state) = wrapDraw state
  draw (ViewChapterState state)   = wrapDraw state

wrapDraw :: DrawState a => a -> String
wrapDraw state = "\x1b[2J\x1b[H"++draw state

react :: UIState -> String -> Maybe UIState
react _ "q"   = Nothing
react (ChooseChapterState state) "j" = Just $ ChooseChapterState $ moveDown state
react (ChooseChapterState state) "k" = Just $ ChooseChapterState $ moveUp state
react state _ = Just state

moveDown :: ChooseChapterState -> ChooseChapterState
moveDown (ChooseChapter global idx) = ChooseChapter global ((idx+1) `mod` length document)
  where
    UIGlobalState document = global

moveUp :: ChooseChapterState -> ChooseChapterState
moveUp (ChooseChapter global idx) = ChooseChapter global ((idx + length document - 1) `mod` length document)
  where
    UIGlobalState document = global
