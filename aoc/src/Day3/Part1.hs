module Day3.Part1 (run) where

import Data.Char (ord)
import Data.List (intersect)
import Data.Maybe (fromJust)

run :: IO ()
run = do
  putStrLn "Running Day 3, Part 1 solution..."
  knapSacks <- readInputFile
  let total = sum $ map priorityOfCommonItem knapSacks
  putStr "The total is "
  print total
  return ()

readInputFile :: IO [String]
readInputFile = do
  contents <- readFile "aoc/src/Day3/input.txt"
  return (lines contents)

priorityOfCommonItem :: String -> Int
priorityOfCommonItem items = fromJust $ getItemPriority $ returnCommonChar firstKnapsackItems secondKnapsackItems
  where
    (firstKnapsackItems, secondKnapsackItems) = splitAt (length items `div` 2) items

returnCommonChar :: String -> String -> Char
returnCommonChar first second = head $ intersect first second

getItemPriority :: Char -> Maybe Int
getItemPriority itemChar
  | itemNum >= 97 = Just $ itemNum - 96
  | itemNum >= 65 = Just $ itemNum - 38
  | otherwise = Nothing
  where
    itemNum = ord itemChar
