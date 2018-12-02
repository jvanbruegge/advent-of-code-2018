module Main where

import D01.Main (solve)

present :: Show a => Maybe a -> String
present = maybe "Error" show

main :: IO ()
main = readFile "D01/input.txt" >>= putStrLn . present . solve
