module Day12.Part2 (run) where

import Data.Char (ord)
import Data.List (elemIndex, elemIndices, find, foldl1', minimumBy, sort)
import Data.Maybe (catMaybes, fromJust, listToMaybe, mapMaybe)
import Data.Ord (comparing)
import Debug.Trace (traceShow)

type Grid = [[Char]]

type Coordinates = (Int, Int)

type Distance = Int

type Distances = [[Int]]

type Visited = [[Bool]]

run :: IO ()
run = do
  putStrLn "Running Day 12, Part 2 solution..."
  grid <- readInputFile
  let startCoords = getAllPositionsForChar 'a' grid ++ getAllPositionsForChar 'S' grid
  let endCoords@(x, y) = fromJust $ getCoords 'E' grid
  let distances = updateMatrix 0 endCoords $ replicate (length grid) (replicate (length (head grid)) (maxDistance grid))
  let visited = replicate (length grid) (replicate (length (head grid)) False)
  let shortestPaths = dijkstras grid visited distances
  let candidates = map (\(x', y') -> shortestPaths !! y' !! x') startCoords
  print $ minimum candidates
  where
    maxDistance lines = length lines * length (head lines) + 1

readInputFile :: IO [String]
readInputFile = do
  contents <- readFile "aoc/src/Day12/input.txt"
  return (lines contents)

hasUnvisited :: Visited -> Bool
hasUnvisited = not . all and

dijkstras :: Grid -> Visited -> Distances -> Distances
dijkstras grid visited distances =
  if hasUnvisited visited
    then do
      let minDistCoordinates@(x, y) = smallestUnvisitedDistanceIndex visited distances
      let minDist = distances !! y !! x
      let updatedVisited = updateMatrix True minDistCoordinates visited
      let neighbours = getTraversableNeighbours minDistCoordinates updatedVisited grid
      dijkstras grid updatedVisited $ foldl (updateDistance minDist) distances neighbours
    else distances
  where
    updateDistance :: Distance -> Distances -> Coordinates -> Distances
    updateDistance curDistance distances coords@(x', y') = do
      let tempDistance = curDistance + 1
      let neighbourDistance = distances !! y' !! x'
      if tempDistance < neighbourDistance then updateMatrix tempDistance coords distances else distances

smallestUnvisitedDistanceIndex :: Visited -> Distances -> Coordinates
smallestUnvisitedDistanceIndex visited distances = indexOfMinimum zippedVisitedDistance
  where
    zippedVisitedDistance = zipWith zip distances visited

minDistance :: [[Int]] -> Int
minDistance a = mod numLines $ fromJust $ elemIndex (foldl1' min flattenedA) flattenedA
  where
    flattenedA = concat a
    numLines = length a

indexOfMinimum :: [[(Int, Bool)]] -> Coordinates
indexOfMinimum visitedAndDistances = (x, y)
  where
    flattened = concat visitedAndDistances
    unvisited = filter (not . snd) flattened
    flattenedIndex = fromJust $ elemIndex (foldl1' min unvisited) flattened
    x = flattenedIndex `mod` length (head visitedAndDistances)
    y = flattenedIndex `div` length (head visitedAndDistances)

updateMatrix :: a -> Coordinates -> [[a]] -> [[a]]
updateMatrix value coords@(x, y) matrix = take y matrix ++ [updatedLine] ++ drop (y + 1) matrix
  where
    originalLine = matrix !! y
    updatedLine = take x originalLine ++ [value] ++ drop (x + 1) originalLine

getTraversableNeighbours :: Coordinates -> Visited -> Grid -> [Coordinates]
getTraversableNeighbours coords@(x, y) visited grid = catMaybes [north, east, south, west]
  where
    getHeight (x', y') = getTerrainHeight (grid !! y' !! x')
    currentHeight = getHeight (x, y)
    northCoords = (x, y - 1)
    notVisited (x', y') = not (visited !! y' !! x')
    north = if y > 0 && notVisited northCoords && (currentHeight <= getHeight northCoords + 1) then Just northCoords else Nothing
    southCoords = (x, y + 1)
    south = if y < length grid - 1 && notVisited southCoords && (currentHeight <= getHeight southCoords + 1) then Just southCoords else Nothing
    westCoords = (x - 1, y)
    west = if x > 0 && notVisited westCoords && (currentHeight <= getHeight westCoords + 1) then Just westCoords else Nothing
    eastCoords = (x + 1, y)
    east = if x < length (head grid) - 1 && notVisited eastCoords && (currentHeight <= getHeight eastCoords + 1) then Just eastCoords else Nothing

getTerrainHeight :: Char -> Int
getTerrainHeight c =
  case c of
    'S' -> 1
    'E' -> 26
    _ -> ord c - 96

getAllPositionsForChar :: Char -> Grid -> [Coordinates]
getAllPositionsForChar c grid = [(x, y) | (y, line) <- zip [0 ..] grid, x <- elemIndices c line]

getCoords :: Char -> Grid -> Maybe Coordinates
getCoords c grid = listToMaybe [(x, y) | (y, line) <- zip [0 ..] grid, x <- elemIndices c line]
