module Day9.Part1 (run) where

import Data.List (find, isPrefixOf)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust, mapMaybe)

type Coordinates = (Int, Int)

newtype Head = Head Coordinates
  deriving (Show)

newtype Tail = Tail Coordinates
  deriving (Show)

data Instruction = Left' Int | Right' Int | Up Int | Down Int

instance Show Instruction where
  show i =
    case i of
      Left' x -> "L " ++ show x
      Right' x -> "R " ++ show x
      Up x -> "U " ++ show x
      Down x -> "D " ++ show x

run :: IO ()
run = do
  putStrLn "Running Day 9, Part 1 solution..."
  input <- readInputFile
  let instructions = mapMaybe parseInstruction input
  let tailHistory = runInstructions (Head (0, 0), Tail (0, 0)) instructions
  print $ length tailHistory

readInputFile :: IO [String]
readInputFile = do
  contents <- readFile "aoc/src/Day9/input.txt"
  return (lines contents)

parseInstruction :: String -> Maybe Instruction
parseInstruction line =
  case head tokens of
    "U" -> Just $ Up $ read $ last tokens
    "D" -> Just $ Down $ read $ last tokens
    "R" -> Just $ Right' $ read $ last tokens
    "L" -> Just $ Left' $ read $ last tokens
    _ -> Nothing
  where
    tokens = splitOn " " line

runInstructions :: (Head, Tail) -> [Instruction] -> [Coordinates]
runInstructions headAndTail = snd . foldl execute (headAndTail, [(0, 0)])

execute :: ((Head, Tail), [Coordinates]) -> Instruction -> ((Head, Tail), [Coordinates])
execute ((head, tail), history) instruction =
  case instruction of
    Up y -> last $ take (y + 1) $ iterate (moveHead 0 (-1)) ((head, tail), history)
    Down y -> last $ take (y + 1) $ iterate (moveHead 0 1) ((head, tail), history)
    Left' x -> last $ take (x + 1) $ iterate (moveHead (-1) 0) ((head, tail), history)
    Right' x -> last $ take (x + 1) $ iterate (moveHead 1 0) ((head, tail), history)
  where
    moveHead :: Int -> Int -> ((Head, Tail), [Coordinates]) -> ((Head, Tail), [Coordinates])
    moveHead xAdjust yAdjust ((Head (hx, hy), tail), tailHistory) = do
      let movedHead = Head (hx + xAdjust, hy + yAdjust)
      let (movedTail, updatedTailHistory) = updateTail (tail, tailHistory) movedHead
      ((movedHead, movedTail), updatedTailHistory)

updateTail :: (Tail, [Coordinates]) -> Head -> (Tail, [Coordinates])
updateTail (tail@(Tail (x, y)), tailHistory) head@(Head (a, b)) =
  if headTailTouching head tail
    then (tail, tailHistory)
    else do
      case headTailDiff head tail of
        (xDiff, 0) -> adjust (dir xDiff 0)
        (0, yDiff) -> adjust (dir 0 yDiff)
        (xDiff, yDiff) -> adjust (dir xDiff yDiff)
  where
    adjust (adjX, adjY) = (Tail (x + adjX, y + adjY), prependIfNotPresent (x + adjX, y + adjY) tailHistory)
    dir xD yD
      | xD < 0 && yD == 0 = (-1, 0)
      | xD > 0 && yD == 0 = (1, 0)
      | xD == 0 && yD < 0 = (0, -1)
      | xD == 0 && yD > 0 = (0, 1)
      | xD < 0 && yD < 0 = (-1, -1)
      | xD < 0 && yD > 0 = (-1, 1)
      | xD > 0 && yD < 0 = (1, -1)
      | xD > 0 && yD > 0 = (1, 1)
      | otherwise = (0, 0)
    prependIfNotPresent coords history = if coords `notElem` history then coords : history else history

headTailDiff :: Head -> Tail -> Coordinates
headTailDiff (Head (hx, hy)) (Tail (tx, ty)) = (hx - tx, hy - ty)

headTailTouching :: Head -> Tail -> Bool
headTailTouching head tail = abs xDiff <= 1 && abs yDiff <= 1
  where
    (xDiff, yDiff) = headTailDiff head tail
