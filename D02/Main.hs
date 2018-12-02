module D02.Main where

import Data.List (sort, group)
import Data.Bifunctor (bimap)

solve :: String -> Int
solve = uncurry (*) . bimap sum sum . unzip . fmap (split (count 2) (count 3)) . words
    where split f g x = (f x, g x)
          count n = min 1 . length . filter (== n) . fmap length . group . sort
