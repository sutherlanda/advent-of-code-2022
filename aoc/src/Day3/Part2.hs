module Day3.Part2 (run) where

import Data.Char (ord)
import Data.List (intersect)
import Data.List.Split
import Data.Maybe (fromJust)

run :: IO ()
run = do
  putStrLn "Running Day 3, Part 2 solution..."
  knapSacks <- readInputFile
  let total = sum $ map priorityOfCommonItem . chunksOf 3 $ knapSacks
  putStr "The total is "
  print total
  return ()

readInputFile :: IO [String]
readInputFile = do
  contents <- readFile "aoc/src/Day3/input.txt"
  return (lines contents)

priorityOfCommonItem :: [String] -> Int
priorityOfCommonItem [first, second, third] = fromJust $ getItemPriority $ returnCommonChar first second third
priorityOfCommonItem _ = 0

returnCommonChar :: String -> String -> String -> Char
returnCommonChar x y z = head $ intersect x $ intersect y z

getItemPriority :: Char -> Maybe Int
getItemPriority itemChar
  | itemNum >= 97 = Just $ itemNum - 96
  | itemNum >= 65 = Just $ itemNum - 38
  | otherwise = Nothing
  where
    itemNum = ord itemChar
