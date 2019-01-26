module Lib.Types where

import qualified Data.Text as T

type Entry = (T.Text, Int)   -- one entry
type Vocabulary = [Entry]    -- a list of entries
