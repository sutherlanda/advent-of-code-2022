module Day8.Part2 (run) where

type Grid = [[Height]]

type Coordinates = (Int, Int)

type Height = Int

run :: IO ()
run = do
  putStrLn "Running Day 8, Part 2 solution..."
  input <- readInputFile
  let grid = map (map (read . (: []))) input
  print $ getMaxScenicScore grid
  return ()

readInputFile :: IO [String]
readInputFile = do
  contents <- readFile "aoc/src/Day8/input.txt"
  return (words contents)

treesVisibleToLeft :: Grid -> Coordinates -> Int
treesVisibleToLeft treeGrid coord@(x, y) = fst result
  where
    result = foldl look (0, True) (reverse (take x (treeGrid !! y)))
    targetHeight = treeGrid !! y !! x
    look (treeCount, continue) height
      | not continue = (treeCount, False)
      | height < targetHeight = (treeCount + 1, continue)
      | otherwise = (treeCount + 1, False)

treesVisibleToRight :: Grid -> Coordinates -> Int
treesVisibleToRight treeGrid coord@(x, y) = fst result
  where
    result = foldl look (0, True) (drop (x + 1) (treeGrid !! y))
    targetHeight = treeGrid !! y !! x
    look (treeCount, continue) height
      | not continue = (treeCount, False)
      | height < targetHeight = (treeCount + 1, continue)
      | otherwise = (treeCount + 1, False)

treesVisibleAbove :: Grid -> Coordinates -> Int
treesVisibleAbove treeGrid coord@(x, y) = fst result
  where
    result = foldl look (0, True) (reverse (take y (map (!! x) treeGrid)))
    targetHeight = treeGrid !! y !! x
    look (treeCount, continue) height
      | not continue = (treeCount, False)
      | height < targetHeight = (treeCount + 1, continue)
      | otherwise = (treeCount + 1, False)

treesVisibleBelow :: Grid -> Coordinates -> Int
treesVisibleBelow treeGrid coord@(x, y) = fst result
  where
    result = foldl look (0, True) (drop (y + 1) (map (!! x) treeGrid))
    targetHeight = treeGrid !! y !! x
    look (treeCount, continue) height
      | not continue = (treeCount, False)
      | height < targetHeight = (treeCount + 1, continue)
      | otherwise = (treeCount + 1, False)

getScenicScore :: Grid -> Coordinates -> Int
getScenicScore treeGrid coord = treesVisibleToLeft treeGrid coord * treesVisibleToRight treeGrid coord * treesVisibleAbove treeGrid coord * treesVisibleBelow treeGrid coord

getMaxScenicScore :: Grid -> Int
getMaxScenicScore treeGrid = maxScenicScore
  where
    len = length treeGrid
    hght = length (head treeGrid)
    coords = [(x, y) | x <- [1 .. (len - 2)], y <- [1 .. (hght - 2)]]
    maxScenicScore = maximum (map (getScenicScore treeGrid) coords)
