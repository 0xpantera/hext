{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( processTextFile
    ) where

import Lib.Types
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


printAllWords :: Vocabulary -> IO ()
printAllWords vocab = do
  putStrLn "All words: "
  TIO.putStrLn $ T.unlines $ map fst vocab


processTextFile :: FilePath -> IO ()
processTextFile fname = do
  text <- TIO.readFile fname
  let vocab = extractVocab text
  printAllWords vocab


-- TODO
-- Remove reliance on IO
extractVocab :: T.Text -> Vocabulary
extractVocab t = map buildEntry $ group $ sort ws
  where
    ws = map T.toCaseFold $ filter (not . T.null) $ map cleanWord $ T.words t
    buildEntry ws@(w:_) = (w, length ws)
    cleanWord = T.dropAround (not . isLetter)

wordsCount :: Vocabulary -> Int

wordsByFrequency :: Vocabulary -> Vocabulary

allWordsReport :: Vocabulary -> T.Text

wordsCountReport :: Vocabulary -> T.Text

frequentWordsReport :: Vocabulary -> Int -> T.Text

processTextFile :: FilePath -> IO ()
