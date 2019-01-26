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


allWordsReport :: Vocabulary -> T.Text
allWordsReport vocab = T.append "\nAll words:\n"
                         $ T.unlines $ map fst vocab


wordsCount :: Vocabulary -> Int
wordsCount vocab = sum $ map snd vocab


wordsByFrequency :: Vocabulary -> Vocabulary
wordsByFrequency = sortBy (comparing $ down . snd)

wordsCountReport :: Vocabulary -> T.Text
wordsCountReport vocab = T.append "\nTotal number of words: "
                           $ T.pack $ show $ wordsCount vocab

frequentWordsReport :: Vocabulary -> Int -> T.Text
frequentWordsReport vocab n = T.append "\nFrequent words:\n"
                              $ T.unlines $ map showEntry $ take n
                              $ wordsByFrequency vocab
  where
    showEntry (t, n) = T.append t $ T.pack $ " - " ++ show n
    

processTextFile :: FilePath -> Int -> IO ()
processTextFile fname n = do
  text <- TIO.readFile fname
  let vocab = extractVocab text
  TIO.putStrLn $ allWordsReport vocab
  TIO.putStrLn $ wordsCountReport vocab
  TIO.putStrLn $ frequentWordsReport vocab n
