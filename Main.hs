module Main where

import Data.Maybe (fromMaybe)

import D02.Main (solve2)

present :: Show a => Maybe a -> String
present = maybe "Error" show

main :: IO ()
main = readFile "D02/input.txt" >>= putStrLn . fromMaybe "Error" . solve2
