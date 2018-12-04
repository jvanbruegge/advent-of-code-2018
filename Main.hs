module Main where

import Data.Maybe (fromMaybe)

import D03.Main (solve)

present :: Show a => Maybe a -> String
present = maybe "Error" show

main :: IO ()
main = readFile "D03/input.txt" >>= putStrLn . present . solve
