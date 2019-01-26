module Main where

import Lib
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment


main :: IO ()
main = do
  args <- getArgs
  case args of
    [fname] -> processTextFile fname
    _ -> putStrLn "Usage: vocab-builder filename"
