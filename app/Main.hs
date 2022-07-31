module Main where
import Control.Monad.IO.Class
import Text.Read
import Data.Maybe

main :: IO ()
main = do
    ints <- readInput
    print ints

readInput :: IO [Int]
readInput = do
    text <- readFile "data/day01.txt"
    return $ mapMaybe readMaybe (lines text)

