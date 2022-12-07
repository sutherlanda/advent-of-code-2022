module Day5.Part2 (run) where

import Data.Char (digitToInt, isSpace)
import Data.List (isPrefixOf)
import Data.Maybe (mapMaybe)

data Stack = Stack Int [Char]

data Row = CrateRow String | InstructionRow Instruction | EmptyRow

instance Show Row where
  show (CrateRow s) = show s
  show (InstructionRow (Instruction amount source target)) = show ("Instruction " ++ show amount ++ " " ++ show source ++ " " ++ show target)
  show EmptyRow = show ""

instance Show Stack where
  show (Stack index str) = show ("Stack " ++ show index ++ " " ++ show str)

instance Show Instruction where
  show (Instruction amount source target) = "move " ++ show amount ++ " " ++ show source ++ " " ++ show target

type Source = Int

type Target = Int

type Amount = Int

data Instruction = Instruction Amount Source Target

run :: IO ()
run = do
  putStrLn "Running Day 5, Part 2 solution..."
  input <- readInputFile
  let rows = map parseRow input
  mapM_ print (runInstructions (getStacks rows) (getInstructions rows))
  print $ getTopCrates $ runInstructions (getStacks rows) (getInstructions rows)

readInputFile :: IO [String]
readInputFile = do
  contents <- readFile "aoc/src/Day5/input.txt"
  return (lines contents)

parseRow :: String -> Row
parseRow line
  | "move" `isPrefixOf` line = InstructionRow $ parseInstruction line
  | '[' `elem` line = CrateRow $ parseCrateRow line
  | otherwise = EmptyRow

parseCrateRow :: String -> String
parseCrateRow line = every 4 (' ' : ' ' : line)
  where
    every n xs =
      case drop (n - 1) xs of
        (y : ys) -> y : every n ys
        [] -> []

parseInstruction :: String -> Instruction
parseInstruction line = Instruction amount source target
  where
    tokens = words line
    extractToken index = read $ tokens !! index
    amount = extractToken 1
    source = extractToken 3
    target = extractToken 5

getStacks :: [Row] -> [Stack]
getStacks rows = fullStacks
  where
    crateRows = [x | CrateRow x <- rows]
    numberOfStacks = length $ last crateRows
    emptyStacks = zipWith Stack [1 .. numberOfStacks] (replicate numberOfStacks "")
    fullStacks = map (\(Stack index _) -> Stack index (trim (reverse (map (!! (index - 1)) crateRows)))) emptyStacks

getInstructions :: [Row] -> [Instruction]
getInstructions rows = [instruction | InstructionRow instruction <- rows]

runInstructions :: [Stack] -> [Instruction] -> [Stack]
runInstructions = foldl applyInstruction

takeFromStack :: Stack -> Amount -> (Stack, String)
takeFromStack (Stack index crates) amount = (Stack index keep, move)
  where
    (keep, move) = splitAt (length crates - amount) crates

putOnStack :: Stack -> String -> Stack
putOnStack (Stack index crates) newCrates = Stack index (crates ++ newCrates)

insertNewStack :: [Stack] -> Stack -> [Stack]
insertNewStack stacks newStack@(Stack index _) = upTo ++ newStack : after
  where
    (upTo, _ : after) = splitAt (index - 1) stacks

applyInstruction :: [Stack] -> Instruction -> [Stack]
applyInstruction stacks instruction@(Instruction amount source target) = insertNewStack (insertNewStack stacks updatedSourceStack) updatedTargetStack
  where
    foo = insertNewStack (insertNewStack stacks updatedSourceStack) updatedTargetStack
    (updatedSourceStack, takenCrates) = takeFromStack (stacks !! (source - 1)) amount
    updatedTargetStack = putOnStack (stacks !! (target - 1)) takenCrates

getTopCrates :: [Stack] -> String
getTopCrates = mapMaybe (\(Stack _ content) -> lastChar content)
  where
    lastChar [] = Nothing
    lastChar str = Just . last $ str

trim :: String -> String
trim = f . f
  where
    f = reverse . dropWhile isSpace
