module D02.Main where

import Data.List (sort, group)
import Data.Maybe (isJust, listToMaybe)
import Data.Bifunctor (bimap)
import Control.Monad ((<=<))
import Control.Arrow ((&&&))

solve :: String -> Int
solve = uncurry (*) . bimap sum sum . unzip . fmap (count 2 &&& count 3) . words
    where count n = min 1 . length . filter (== n) . fmap length . group . sort

solve2 :: String -> Maybe String
solve2 = listToMaybe <=< sequenceA . filter isJust . fmap checkSimilar . cartesian . words
    where cartesian xs = [(x, y) | x <- xs, y <- xs, x /= y]

checkSimilar :: (String, String) -> Maybe String
checkSimilar (a, b) = go False a b
    where go _ [] _ = Just ""
          go _ _ [] = Just ""
          go False (x:xs) (y:ys)
              | x == y = (x:) <$> go False xs ys
              | otherwise = go True xs ys
          go True (x:xs) (y:ys)
              | x == y = (x:) <$> go True xs ys
              | otherwise = Nothing
