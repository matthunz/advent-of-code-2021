module Day2 where

run :: IO ()
run = do
  text <- readFile "data/day02.txt"
  let ins = parseIns text
  print $ solutionA ins
  print $ solutionB ins

data Instruction = Forward Int | Up Int | Down Int deriving (Show)

parseIns :: String -> [Instruction]
parseIns text = map g (f $ words text)
  where
    f :: [String] -> [([Char], Int)]
    f (op : distance : xs) = (op, read distance) : f xs
    f _ = []
    g :: ([Char], Int) -> Instruction
    g ("forward", d) = Forward d
    g ("up", d) = Up d
    g ("down", d) = Down d
    g _ = error "Invalid instruction"

solutionA :: [Instruction] -> Int
solutionA instructions = h $ foldl g (0, 0, 0) instructions
  where
    g (x, y, a) (Forward d) = (x + d, a * d + y, a)
    g (x, y, a) (Up d) = (x, y, a - d)
    g (x, y, a) (Down d) = (x, y, a + d)
    h (x, y, _) = x * y

solutionB :: [Instruction] -> Int
solutionB instructions = h $ foldl g (0, 0) instructions
  where
    g (x, y) (Forward d) = (x + d, y)
    g (x, y) (Up d) = (x, y - d)
    g (x, y) (Down d) = (x, y + d)
    h (x, y) = x * y
