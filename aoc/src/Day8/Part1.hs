module Day8.Part1 (run) where

type Grid = [[Height]]

type Coordinates = (Int, Int)

type Height = Int

run :: IO ()
run = do
  putStrLn "Running Day 8, Part 1 solution..."
  input <- readInputFile
  let grid = map (map (read . (: []))) input
  print $ countVisibleTrees grid
  return ()

readInputFile :: IO [String]
readInputFile = do
  contents <- readFile "aoc/src/Day8/input.txt"
  return (words contents)

isTreeVisibleFromLeft :: Grid -> Coordinates -> Bool
isTreeVisibleFromLeft treeGrid coord@(x, y) = result
  where
    result = foldl look True (take x (treeGrid !! y))
    targetHeight = treeGrid !! y !! x
    look visible height = visible && height < targetHeight

isTreeVisibleFromRight :: Grid -> Coordinates -> Bool
isTreeVisibleFromRight treeGrid coord@(x, y) = result
  where
    result = foldl look True (drop (x + 1) (treeGrid !! y))
    targetHeight = treeGrid !! y !! x
    look visible height = visible && height < targetHeight

isTreeVisibleFromTop :: Grid -> Coordinates -> Bool
isTreeVisibleFromTop treeGrid coord@(x, y) = result
  where
    result = foldl look True (take y (map (!! x) treeGrid))
    targetHeight = treeGrid !! y !! x
    look visible height = visible && height < targetHeight

isTreeVisibleFromBottom :: Grid -> Coordinates -> Bool
isTreeVisibleFromBottom treeGrid coord@(x, y) = result
  where
    result = foldl look True (drop (y + 1) (map (!! x) treeGrid))
    targetHeight = treeGrid !! y !! x
    look visible height = visible && height < targetHeight

isTreeVisible :: Grid -> Coordinates -> Bool
isTreeVisible treeGrid coord = isTreeVisibleFromLeft treeGrid coord || isTreeVisibleFromRight treeGrid coord || isTreeVisibleFromTop treeGrid coord || isTreeVisibleFromBottom treeGrid coord

countVisibleTrees :: Grid -> Int
countVisibleTrees treeGrid = countPerimeterTrees treeGrid + innerCount
  where
    len = length treeGrid
    hght = length (head treeGrid)
    innerCoords = [(x, y) | x <- [1 .. (len - 2)], y <- [1 .. (hght - 2)]]
    innerCount = length (filter (== True) (map (isTreeVisible treeGrid) innerCoords))

countPerimeterTrees :: Grid -> Int
countPerimeterTrees treeGrid = (len * hght) - ((len - 2) * (hght - 2))
  where
    len = length treeGrid
    hght = length (head treeGrid)
