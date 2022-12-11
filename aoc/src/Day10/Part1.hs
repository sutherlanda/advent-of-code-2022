module Day10.Part1 (run) where

data Instruction = NoOp | AddX Int

type Cycle = Int

type X = Int

type Accumulator = [(Instruction, X)]

instance Show Instruction where
  show i =
    case i of
      NoOp -> "noop"
      AddX v -> "addx " ++ show v

run :: IO ()
run = do
  putStrLn "Running Day 10, Part 1 solution..."
  input <- readInputFile
  let instructions = map parseInstruction input
  let (accum, _) = foldl execInstruction ([], 1) instructions
  let intervals = [20, 60, 100, 140, 180, 220]
  let samples = map (\i -> i * snd (accum !! (i - 1))) intervals
  print $ sum samples

execInstruction :: (Accumulator, X) -> Instruction -> (Accumulator, X)
execInstruction (accum, x) instruction =
  case instruction of
    NoOp -> (accum ++ [(instruction, x)], x)
    AddX value -> (accum ++ [(instruction, x)] ++ [(instruction, x)], x + value)

readInputFile :: IO [String]
readInputFile = do
  contents <- readFile "aoc/src/Day10/input.txt"
  return (lines contents)

parseInstruction :: String -> Instruction
parseInstruction s =
  case s of
    "noop" -> NoOp
    _ -> AddX $ read $ words s !! 1
