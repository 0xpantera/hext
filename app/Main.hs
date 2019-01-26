{-# LANGUAGE OverloadedStrings #-}
module Main where

import Lib
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment


main :: IO ()
main = do
  args <- getArgs
  case args of
    [fname, n] -> processTextFile fname (read n)
    _ -> putStrLn "Usage: vocab-builder filename number_of_frequent_words"
