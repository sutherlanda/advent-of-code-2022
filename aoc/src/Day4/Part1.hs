module Day4.Part1 (run) where

import Data.Char (isSpace)
import Data.List.Split (splitOn)

type Start = Int

type Finish = Int

data Range = Range Start Finish

data RangePair = RangePair Range Range

instance Show Range where
  show (Range start finish) = show start ++ "-" ++ show finish

instance Show RangePair where
  show (RangePair first second) = show first ++ "," ++ show second

run :: IO ()
run = do
  putStrLn "Running Day 4, Part 1 solution..."
  lines <- readInputFile
  let rangePairs = map parseRangePair lines
  let containingRangePairs = filter doesRangeContainRange rangePairs
  print $ length containingRangePairs

readInputFile :: IO [String]
readInputFile = do
  contents <- readFile "aoc/src/Day4/input.txt"
  return (lines contents)

parseRangePair :: String -> RangePair
parseRangePair line = RangePair (parseRange first) (parseRange second)
  where
    [first, second] = splitOn "," line

parseRange :: String -> Range
parseRange line = Range (read start) (read finish)
  where
    [start, finish] = splitOn "-" line

doesRangeContainRange :: RangePair -> Bool
doesRangeContainRange (RangePair (Range start1 finish1) (Range start2 finish2))
  | start1 <= start2 && finish1 >= finish2 = True
  | start2 <= start1 && finish2 >= finish1 = True
  | otherwise = False

trim :: String -> String
trim = f . f
  where
    f = reverse . dropWhile isSpace
