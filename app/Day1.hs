module Day1 where

import Data.Maybe (mapMaybe)
import Text.Read (readMaybe)

run :: IO ()
run = do
  ints <- parse
  print $ solutionA ints
  print $ solutionB ints

parse :: IO [Int]
parse = readFile "data/day01.txt" >>= traverse readIO . lines

solutionA :: [Int] -> Int
solutionA = length . filter (> 0) . diff
  where
    diff (a1 : a2 : as) = a2 - a1 : diff (a2 : as)
    diff _ = []

solutionB :: [Int] -> Int
solutionB = solutionA . slidingSum
  where
    slidingSum xs = zipWith3 (\x1 x2 x3 -> x1 + x2 + x3) xs (tail xs) (drop 2 xs)
