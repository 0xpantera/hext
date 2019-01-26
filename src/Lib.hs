module Lib
    ( processText
    ) where

import Data.Char
import Data.List
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment


dropPunctuation :: T.Text -> [T.Text]
dropPunctuation text = map cutTrailing $ T.words text
  where cutTrailing = (T.dropAround $ not . isLetter)


dropNullLower :: [T.Text] -> [T.Text]
dropNullLower text = map T.toCaseFold $ nonNulls text
  where nonNulls = filter (not . T.null)


getUniqueWords :: [T.Text] -> [T.Text]
getUniqueWords text = map head $ group $ sort text


processText :: T.Text -> [T.Text]
processText text = (getUniqueWords . dropNullLower . dropPunctuation) text
