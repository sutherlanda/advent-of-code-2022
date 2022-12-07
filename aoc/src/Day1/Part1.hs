module Day1.Part1 (run) where

import Data.Char (isSpace)
import Data.List.Split (splitWhen)

type Calories = Int

run :: IO ()
run = do
  putStrLn "Running Day 1, Part 1 solution..."
  lines <- readInputFile
  let calorieCounts = countCalories lines
  putStr "The elf carrying the most calories is carrying "
  print $ maximum calorieCounts
  print " calories"
  return ()

readInputFile :: IO [String]
readInputFile = do
  contents <- readFile "aoc/src/Day1/input.txt"
  return (lines contents)

countCalories :: [String] -> [Int]
countCalories lines = map (sum . map read) calorieGroups
  where
    calorieGroups = splitWhen (null . trim) lines

trim :: String -> String
trim = f . f
  where
    f = reverse . dropWhile isSpace
