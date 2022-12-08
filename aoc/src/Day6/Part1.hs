module Day6.Part1 (run) where

import Data.List.Split (splitOn)

run :: IO ()
run = do
  putStrLn "Running Day 6, Part 1 solution..."
  input <- readFile "aoc/src/Day6/input.txt"
  print $ process input

process :: String -> Int
process = snd . last . takeWhile (\(str, _) -> length str < 4) . scanl reader ("", 1)
  where
    reader (prev, totalCount) char
      | char `elem` prev = ((splitOn [char] prev !! 1) ++ [char], totalCount + 1)
      | otherwise = (prev ++ [char], totalCount + 1)
