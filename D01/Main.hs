module D01.Main where

import Text.Read (readMaybe)

solve :: String -> Maybe Int
solve = fmap sum . traverse (readMaybe . filter (/= '+')) . words
