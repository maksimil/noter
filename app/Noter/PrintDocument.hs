module Noter.PrintDocument (documentString, defaultDocumentStyle, DocumentStyle) where

import           Noter.ParseDocument

type Index = Int

data DocumentStyle = DocumentStyle
  { formatHeading :: Index -> String -> String,
    formatLine    :: String -> String
  }

defaultFormatHeading :: Index -> String -> String
defaultFormatHeading idx heading =
  "\x1b[34m" ++ show idx ++ ". " ++ heading ++ "\x1b[0m"

defaultDocumentStyle :: DocumentStyle
defaultDocumentStyle = DocumentStyle {formatHeading = defaultFormatHeading, formatLine = id}

joinList :: [a] -> [[a]] -> [a]
joinList sep = foldl1 (\acc sublist -> acc ++ sep ++ sublist)

documentString :: DocumentStyle -> Document -> String
documentString style = joinList "\n\n" .  foldl (\acc idxPart -> acc++headingString idxPart) [] . zip [1..]
  where
    formatHeadingStyle = formatHeading style
    formatLineStyle = formatLine style

    headingString :: (Index, (String, [String])) -> [String]
    headingString (idx, (heading, contents)) = formatHeadingStyle idx heading : (formatLineStyle <$> contents)
