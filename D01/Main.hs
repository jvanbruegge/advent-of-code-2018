module D01.Main where

import Text.Read (readMaybe)
import Control.Monad ((<=<))
import qualified Data.Set as S

parseInput :: String -> Maybe [Int]
parseInput = traverse (readMaybe . filter (/= '+')) . words

solve :: String -> Maybe Int
solve = fmap sum . parseInput

solve2 :: String -> Maybe Int
solve2 = snd . go S.empty <=< fmap (scanl (+) 0 . cycle) . parseInput
    where go s [] = (s, Nothing)
          go s (x:xs) = if x `S.member` s then (s, Just x) else go (S.insert x s) xs
