module Day10.Part2 (run) where

import Data.List.Split (chunksOf, splitOn)

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
  putStrLn "Running Day 10, Part 2 solution..."
  input <- readInputFile
  let instructions = map parseInstruction input
  let (accum, _) = foldl execInstruction ([], 1) instructions

  let cycledInstructions = zip accum [0 .. 239]
  let pixels = chunksOf 40 $ reverse $ foldl draw [] cycledInstructions
  mapM_ print pixels

draw :: String -> ((Instruction, X), Cycle) -> String
draw pixels ((instr, x), cycle)
  | x >= (cycle `mod` 40) - 1 && x <= (cycle `mod` 40) + 1 = '#' : pixels
  | otherwise = '.' : pixels

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
