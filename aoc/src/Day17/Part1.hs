{-# LANGUAGE RecordWildCards #-}

module Day17.Part1 where

import Data.Bifunctor (bimap, first, second)
import Data.List (concatMap, findIndex, intersperse)
import Data.Maybe (fromMaybe, isNothing, mapMaybe)

data Shape = Minus | Plus | BackwardsL | VLine | Block
  deriving (Show)

data Rock = Rock
  { rockShape :: Shape,
    rockPosition :: (Int, Int)
  }
  deriving (Show)

data Chamber = Chamber
  { chamberFree :: [[Bool]],
    chamberRockCount :: Int,
    chamberCurrentRock :: Maybe Rock
  }

instance Show Chamber where
  show Chamber {..} = concatMap ("\n" ++) (addFloor (addSides (addCurrentRock (map (map toChar) chamberFree))))
    where
      toChar free = if free then '.' else '#'
      addSides lst = map (\s -> '|' : s ++ "|") lst
      addFloor lst = lst ++ ["+" ++ replicate 7 '-' ++ "+"]
      addCurrentRock lst = foldl (\lst' (x', y') -> replaceNth y' (replaceNth x' '@' (lst' !! y')) lst') lst (maybe [] rockCoordinates chamberCurrentRock)

replaceNth :: Int -> a -> [a] -> [a]
replaceNth idx val lst = take idx lst ++ [val] ++ drop (idx + 1) lst

type Coordinates = (Int, Int)

type Offset = Coordinates

data Direction = DLeft | DRight | Down
  deriving (Eq, Show)

type WindGustPattern = String

width = 7 :: Int

maxRockCount = 2022 :: Int

run :: IO ()
run = do
  putStrLn "Running Day 17, Part 1 solution..."
  input <- readInputFile
  let chamber = Chamber [] 0 Nothing
  let dirList = cycle (intersperse Down (windPattern input) ++ [Down])
  let result = progress dirList chamber
  print $ length (chamberFree result)
  return ()

readInputFile :: IO String
readInputFile = readFile "aoc/src/Day17/input.txt"

windPattern :: String -> [Direction]
windPattern = mapMaybe cToDir
  where
    cToDir c =
      case c of
        '>' -> Just DRight
        '<' -> Just DLeft
        _ -> Nothing

rockCoordinates :: Rock -> [(Int, Int)]
rockCoordinates (Rock shape (x, y)) =
  map (bimap (+ x) (+ y)) $ case shape of
    Minus -> [(0, 0), (1, 0), (2, 0), (3, 0)]
    Plus -> [(1, 0), (0, 1), (1, 1), (2, 1), (1, 2)]
    BackwardsL -> [(2, 0), (2, 1), (0, 2), (1, 2), (2, 2)]
    VLine -> [(0, 0), (0, 1), (0, 2), (0, 3)]
    Block -> [(0, 0), (0, 1), (1, 1), (1, 0)]

rockSequence :: [Rock]
rockSequence = [Rock Minus (2, 0), Rock Plus (2, 0), Rock BackwardsL (2, 0), Rock VLine (2, 0), Rock Block (2, 0)]

progress :: [Direction] -> Chamber -> Chamber
progress [] chamber = chamber
progress dirs@(direction : rest) chamber@Chamber {..} =
  if chamberRockCount == maxRockCount
    then chamber
    else case (chamberCurrentRock, direction) of
      (Nothing, _) -> progress dirs $ nextRock chamber
      (Just rock, Down) -> progress rest $ moveDown chamber rock
      (Just rock, hDir) -> progress rest $ moveHorizontally chamber rock hDir

moveHorizontally :: Chamber -> Rock -> Direction -> Chamber
moveHorizontally chamber@Chamber {..} rock direction =
  if (not . all isFree) nextRockCoords
    then chamber
    else chamber {chamberCurrentRock = Just (rock {rockPosition = updatedRockPos})}
  where
    offset = if direction == DLeft then -1 else 1
    updatedRockPos = first (+ offset) (rockPosition rock)
    rockCoords = rockCoordinates rock
    nextRockCoords = map (first (+ offset)) rockCoords
    isFree (x', y') = x' >= 0 && x' < width && chamberFree !! y' !! x'

moveDown :: Chamber -> Rock -> Chamber
moveDown chamber@Chamber {..} rock =
  if (not . all isFree) nextRockCoords
    then chamber {chamberCurrentRock = Nothing, chamberFree = chamberFree', chamberRockCount = chamberRockCount + 1}
    else chamber {chamberCurrentRock = Just (rock {rockPosition = updatedRockPos})}
  where
    updatedRockPos = second (+ 1) (rockPosition rock)
    rockCoords = rockCoordinates rock
    nextRockCoords = map (second (+ 1)) rockCoords
    isFree (x', y') = y' >= 0 && y' < length chamberFree && chamberFree !! y' !! x'
    unfreeRockSpace = foldl (\c (x', y') -> replaceNth y' (replaceNth x' False (c !! y')) c) chamberFree rockCoords
    chamberFree' = filter (not . all (== True)) unfreeRockSpace

nextRock :: Chamber -> Chamber
nextRock Chamber {..} = Chamber chamberFree' chamberRockCount (Just currentRock')
  where
    currentRock' = rockSequence !! (chamberRockCount `mod` 5)
    chamberFree' = replicate heightToAdd (replicate width True) ++ chamberFree
    heightToAdd = rockHeight currentRock' + 3

rockHeight :: Rock -> Int
rockHeight rock@Rock {..} = maximum yCoords - minimum yCoords + 1
  where
    yCoords = map snd (rockCoordinates rock)
