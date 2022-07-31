module Main where

import Control.Monad.IO.Class
import Data.Maybe (mapMaybe)
import Text.Read

main :: IO ()
main = do
  ints <- readInput
  print $ solutionA ints
  print $ solutionB ints

readInput :: IO [Int]
readInput = do
  text <- readFile "data/day01.txt"
  return $ mapMaybe readMaybe (lines text)

solutionA :: [Int] -> Int
solutionA = length . filter (> 0) . diff
  where
    diff (a1 : a2 : as) = a2 - a1 : diff (a2 : as)
    diff _ = []

solutionB :: [Int] -> Int
solutionB = solutionA . slidingSum
  where
    slidingSum xs = zipWith3 (\x1 x2 x3 -> x1 + x2 + x3) xs (tail xs) (drop 2 xs)
