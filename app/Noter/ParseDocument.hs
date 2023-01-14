module Noter.ParseDocument (readDocument, Document) where

import           Data.Bifunctor (second)
import           Noter.Utils

type Heading = String
type Contents = [String]
type Document = [(Heading, Contents)]

emptyHeading :: String
emptyHeading = "Base"

data Token
  = HeadLine String
  | DataLine String
  | EmptyLine
  deriving (Show, Eq)

trimFront :: (a-> Bool)->[a]->[a]
trimFront _ []   = []
trimFront fn lst = if' (fn $ head lst) (trimFront fn $ tail lst) lst

trimBack :: (a-> Bool)->[a]->[a]
trimBack _ []   = []
trimBack fn lst = if' (fn $ last lst) (trimFront fn $ init lst) lst

trim :: (a->Bool) -> [a] -> [a]
trim fn = trimFront fn . trimBack fn

trimString :: String -> String
trimString = trim (==' ')

taggedSplit :: (a->Maybe b) -> b -> [a] -> [(b,[a])]
taggedSplit fn initialHeading = foldl (\acc c -> folder acc c (fn c)) []
  where
    -- folder :: [(b,[a])] -> a -> Maybe b -> [(b,[a])]
    folder acc _ (Just tag) = acc++[(tag, [])]
    folder [] el Nothing    = [(initialHeading, [el])]
    folder acc el Nothing   = init acc ++ [combine (last acc) el]

    combine :: (b, [a]) -> a -> (b,[a])
    combine (tag, lst) el = (tag, lst ++ [el])

splitList :: (a->Bool)->[a]->[[a]]
splitList fn = fmap snd . taggedSplit (\x -> if' (fn x) (Just ()) Nothing) ()

splitLines :: String -> [String]
splitLines = splitList (=='\n')

tokenize :: [String] -> [Token]
tokenize = fmap lineMap
  where
    lineMap ""   = EmptyLine
    lineMap line = if' (head line == '#') (HeadLine . trim (==' ') . tail) DataLine line

documentFromTokens :: [Token] -> Document
documentFromTokens = fmap (second parseContents) . taggedSplit getHeading emptyHeading
  where
    getHeading (HeadLine line) = Just line
    getHeading _               = Nothing

parseContents :: [Token] -> [String]
parseContents = filter (/="") . fmap (foldl combineLines "") . splitList (==EmptyLine) . trim (==EmptyLine)
  where
    combineLines "" (DataLine line)  = trimString line
    combineLines acc (DataLine line) = acc++" "++trimString line
    combineLines acc _               = acc

readDocument :: String -> Document
readDocument = documentFromTokens . tokenize . splitLines
