module Main where

import D02.Main (solve)

present :: Show a => Maybe a -> String
present = maybe "Error" show

main :: IO ()
main = readFile "D02/input.txt" >>= print . solve
