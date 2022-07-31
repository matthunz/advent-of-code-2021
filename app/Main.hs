module Main where

import Control.Monad.IO.Class
import Data.Maybe (mapMaybe)
import Text.Read

data Instruction = Forward Int | Up Int | Down Int deriving (Show)

instructions text = map g (f $ words text)
  where
    f :: [String] -> [([Char], Int)]
    f (op : distance : xs) = (op, read distance) : f xs
    f _ = []
    g :: ([Char], Int) -> Instruction
    g ("forward", d) = Forward d
    g ("up", d) = Up d
    g ("down", d) = Down d

main :: IO ()
main = do
  text <- readFile "data/day02.txt"
  print $ h $ foldl g (0, 0, 0) (instructions text)
  where
    g (x, y, a) (Forward d) = (x + d, a * d + y, a)
    g (x, y, a) (Up d) = (x, y, a - d)
    g (x, y, a) (Down d) = (x, y, a + d)

    h (x, y, _) = x * y

main3 :: IO ()
main3 = do
  text <- readFile "data/day02.txt"
  print $ h $ foldl g (0, 0) (instructions text)
  where
    g (x, y) (Forward d) = (x + d, y)
    g (x, y) (Up d) = (x, y - d)
    g (x, y) (Down d) = (x, y + d)

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
