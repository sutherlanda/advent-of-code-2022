module Day9.Part2 (run) where

import Data.List (find, isPrefixOf)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust, mapMaybe)
import Debug.Trace (traceShow)

type Coordinates = (Int, Int)

type Position = Int

data Knot = Knot Position Coordinates

type Head = Knot

type Tail = Knot

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
  putStrLn "Running Day 9, Part 2 solution..."
  input <- readInputFile
  let instructions = mapMaybe parseInstruction input
  let tailHistory = runInstructions (Knot 0 (0, 0), map (\position -> Knot position (0, 0)) [1 .. 9]) instructions
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

runInstructions :: (Head, [Tail]) -> [Instruction] -> [Coordinates]
runInstructions rope = snd . foldl execute (rope, [(0, 0)])

execute :: ((Head, [Tail]), [Coordinates]) -> Instruction -> ((Head, [Tail]), [Coordinates])
execute ((head', tails), history) instruction =
  case instruction of
    Up y -> last $ take (y + 1) $ iterate (moveHead 0 (-1)) ((head', tails), history)
    Down y -> last $ take (y + 1) $ iterate (moveHead 0 1) ((head', tails), history)
    Left' x -> last $ take (x + 1) $ iterate (moveHead (-1) 0) ((head', tails), history)
    Right' x -> last $ take (x + 1) $ iterate (moveHead 1 0) ((head', tails), history)
  where
    moveHead :: Int -> Int -> ((Head, [Tail]), [Coordinates]) -> ((Head, [Tail]), [Coordinates])
    moveHead xAdjust yAdjust ((Knot position (hx, hy), tails), tailHistory) = do
      let movedHead = Knot position (hx + xAdjust, hy + yAdjust)
      let (_ : moveResult) = scanl updateKnot (movedHead, tailHistory) tails
      let movedTails = map fst moveResult
      let updatedHistory = snd $ last moveResult
      ((movedHead, movedTails), updatedHistory)

updateKnot :: (Knot, [Coordinates]) -> Knot -> (Knot, [Coordinates])
updateKnot (updatedKnot@(Knot _ (x, y)), tailHistory) knotToUpdate@(Knot position (a, b)) =
  if headTailTouching updatedKnot knotToUpdate
    then (knotToUpdate, tailHistory)
    else do
      case headTailDiff updatedKnot knotToUpdate of
        (xDiff, 0) -> adjust (dir xDiff 0)
        (0, yDiff) -> adjust (dir 0 yDiff)
        (xDiff, yDiff) -> adjust (dir xDiff yDiff)
  where
    adjust (adjX, adjY) = (Knot position (a + adjX, b + adjY), prepend position (a + adjX, b + adjY) tailHistory)
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
    prepend position coords history = if position == 9 && coords `notElem` history then coords : history else history

headTailDiff :: Head -> Tail -> Coordinates
headTailDiff (Knot _ (hx, hy)) (Knot _ (tx, ty)) = (hx - tx, hy - ty)

headTailTouching :: Head -> Tail -> Bool
headTailTouching head tail = abs xDiff <= 1 && abs yDiff <= 1
  where
    (xDiff, yDiff) = headTailDiff head tail
