module Main where

import Control.Monad.IO.Class ()
import Data.Foldable (traverse_)
import Data.Maybe (mapMaybe)
import qualified Day1
import qualified Day2
import System.Environment (getArgs)
import Text.Read ()

main :: IO ()
main = do
  args <- getArgs
  traverse_ run (parse args)

parse :: (Num a, Enum a, Read a) => [String] -> [a]
parse [] = [1 .. 2]
parse days = map read days

run :: Int -> IO ()
run 1 = Day1.run
run 2 = Day2.run
run _ = error "Unknown day"
