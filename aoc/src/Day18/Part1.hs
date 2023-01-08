module Day18.Part1 where

import Data.List.Split (splitOn)
import qualified Data.Set as S

type Cube = (Int, Int, Int)

type CoordinateSet = S.Set Int

type CoordinateSets3D = (CoordinateSet, CoordinateSet, CoordinateSet)

run :: IO ()
run = do
  putStrLn "Running Day 18, Part 1 solution..."
  input <- readInputFile
  let cubes' = cubes input
  let coordSets' = coordSets cubes'
  print $ sum $ map (surfaceArea coordSets') cubes'
  return ()

readInputFile :: IO String
readInputFile = readFile "aoc/src/Day18/input.txt"

cubes :: String -> [Cube]
cubes s = map cube $ words s
  where
    cube str = toTuple $ map read $ splitOn "," str
    toTuple [x, y, z] = (x, y, z)
    toTuple _ = undefined

coordSets :: [Cube] -> S.Set Cube
coordSets = foldl (flip S.insert) S.empty

surfaceArea :: S.Set Cube -> Cube -> Int
surfaceArea cubeSet cube@(x, y, z) = 6 - length (filter (== True) check)
  where
    offsets = [(-1, 0, 0), (1, 0, 0), (0, -1, 0), (0, 1, 0), (0, 0, -1), (0, 0, 1)]
    check = map (exists . add cube) offsets
    add (x', y', z') (x'', y'', z'') = (x' + x'', y' + y'', z' + z'')
    exists coords = S.member coords cubeSet
