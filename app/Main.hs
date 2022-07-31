module Main where

import Control.Monad.IO.Class
import Data.Maybe (mapMaybe)
import Text.Read

data Instruction = Instruction String Int deriving (Show)

main :: IO ()
main = do
  text <- readFile "data/day02.txt"
  print $ h $ foldl g (0, 0) (f $ words text)
  where
    f :: [String] -> [Instruction]
    f (op : distance : xs) = Instruction op (read distance) : f xs
    f _ = []
    g (x, y) (Instruction "forward" d) = (x + d, y)
    g (x, y) (Instruction "up" d) = (x, y - d)
    g (x, y) (Instruction "down" d) = (x, y + d)
    g (x, y) _ = (x, y)
    h (x, y) = x * y

main2 = do
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
