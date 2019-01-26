module Main where

import Lib
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment


main :: IO ()
main = do
  [fname] <- getArgs
  text <- TIO.readFile fname
  let ws = processText text
  TIO.putStrLn $ T.unwords ws
  print $ length ws
