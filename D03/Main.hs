module D03.Main where

import Data.List (sort, group)
import Text.Read (readMaybe)
import Control.Monad ((<=<))
import qualified Data.Set as S

data Claim = Claim {
        claimID :: Int,
        left, top, width, height :: Int
    } deriving Show

(<$<) :: Functor f => (b -> c) -> (a -> f b) -> a -> f c
(<$<) f = (fmap f .)
infix 8 <$<

parseInput :: String -> Maybe [Claim]
parseInput = traverse parseLine . lines
    where parseLine :: String -> Maybe Claim
          parseLine = toClaim <=< traverse readMaybe . words . fmap spToSpace
          toClaim [x, l, r, w, h] = Just $ Claim x l r w h
          toClaim _ = Nothing
          spToSpace c = if c `elem` ['#', '@', ',', ':', 'x'] then ' ' else c

calculateIntersection :: Claim -> Claim -> S.Set (Int, Int) -> S.Set (Int, Int)
calculateIntersection a b = flip (foldr S.insert)
            [(x, y) | x <- [minX..maxX - 1], y <- [minY..maxY - 1]]
    where minX = max (left a) (left b)
          maxX = min (left a + width a) (left b + width b)
          minY = max (top a) (top b)
          maxY = min (top a + height a) (top b + height b)

solve :: String -> Maybe Int
solve = S.size . foldr (uncurry calculateIntersection) S.empty . cartesian <$< parseInput
    where cartesian xs = let l = zip xs [0..]
                         in [(x, y) | (x, i) <- l, y <- fst <$> takeWhile ((< i) . snd) l]
