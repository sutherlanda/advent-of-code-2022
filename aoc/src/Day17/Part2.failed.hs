{-# LANGUAGE RecordWildCards #-}

module Day17.Part2 where

import Data.Bifunctor (bimap, first, second)
import qualified Data.Hashable as H
import Data.List (concatMap, findIndex, intersperse)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, isNothing, mapMaybe)

data Shape = Minus | Plus | BackwardsL | VLine | Block
  deriving (Show)

instance H.Hashable Shape where
  hashWithSalt s shape = H.hashWithSalt s (show shape)

data Rock = Rock
  { rockShape :: Shape,
    rockPosition :: (Int, Int)
  }
  deriving (Show)

instance H.Hashable Rock where
  hashWithSalt s Rock {..} = H.hashWithSalt s rockShape

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

instance H.Hashable Direction where
  hashWithSalt s dir = H.hashWithSalt s (show dir)

type WindGustPattern = String

width = 7 :: Int

maxRockCount = 2022 :: Int

run :: IO ()
run = do
  putStrLn "Running Day 17, Part 2 solution..."
  input <- readInputFile
  let chamber = Chamber [] 0 Nothing
  let dirList = intersperse Down (windPattern input) ++ [Down]
  let result = progress M.empty dirList dirList chamber
  print $ length (chamberFree result)
  return ()

readInputFile :: IO String
readInputFile = readFile "aoc/src/Day17/test.txt"

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

fingerPrint :: Rock -> [Direction] -> Int
fingerPrint rock@Rock {..} directions = H.hash (rock, directions)

updateCache :: Rock -> [Direction] -> M.Map Int Int -> M.Map Int Int
updateCache rock dirs m = M.insert (fingerPrint rock dirs) current m
  where
    current = maybe 0 (+ 1) $ M.lookup key m
    key = fingerPrint rock dirs

progress :: M.Map Int Int -> [Direction] -> [Direction] -> Chamber -> Chamber
progress patternCache fullDirs [] chamber = progress patternCache fullDirs fullDirs chamber
progress patternCache fullDirs dirs@(direction : rest) chamber@Chamber {..} =
  if chamberRockCount == maxRockCount
    then chamber
    else case (chamberCurrentRock, direction) of
      (Nothing, _) -> progress patternCache fullDirs dirs $ nextRock chamber
      (Just rock, Down) -> progress patternCache fullDirs rest $ moveDown chamber rock
      (Just rock, hDir) -> progress patternCache fullDirs rest $ moveHorizontally chamber rock hDir

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
