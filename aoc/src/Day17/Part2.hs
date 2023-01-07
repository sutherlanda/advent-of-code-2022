{-# LANGUAGE RecordWildCards #-}

--[># LANGUAGE TupleSections #<]

module Day17.Part2 where

import Control.Monad.State
import Data.Bifunctor (bimap, first, second)
import qualified Data.Hashable as H
import Data.List (concatMap, findIndex, intersperse)
import Data.List.Split (splitOn)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, isNothing, mapMaybe)
import qualified Data.Sequence as Seq
import Debug.Trace (traceShow)

data Shape = Minus | Plus | BackwardsL | VLine | Block
  deriving (Show, Eq, Ord)

instance H.Hashable Shape where
  hashWithSalt s shape = H.hashWithSalt s (show shape)

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
  deriving (Eq)

type WindGustPattern = String

type CacheKey = (Maybe Shape, Int, Int)

type Cache = ([CacheKey], Int, Int)

width = 7 :: Int

--maxRockCount = 1000000000000 :: Int

maxRockCount = 2022 :: Int

run :: IO ()
run = do
  putStrLn "Running Day 17, Part 2 solution..."
  input <- readInputFile
  let chamber = Chamber [] 0 Nothing
  let dirList = intersperse Down (windPattern input) ++ [Down]
  let (result, cache@(seq, _, _)) = foo dirList chamber
  print cache
  let cycleResult@(nonCycle, cycle) = findCycle (last seq) seq
  print cycleResult
  let (initRocks, initHeight) = foldl (\(rCount, hCount) (_, r, h) -> (rCount + r, hCount + h)) (0, 0) nonCycle
  print (initRocks, initHeight)
  let (rocksPerCycle, heightPerCycle) = foldl (\(rCount, hCount) (_, r, h) -> (rCount + r, hCount + h)) (0, 0) cycle
  print (rocksPerCycle, heightPerCycle)
  let remainingRocks = maxRockCount - initRocks
  let cyclesNeeded = remainingRocks `div` rocksPerCycle
  let extraRocksNeeded = remainingRocks `mod` rocksPerCycle
  print cyclesNeeded
  print extraRocksNeeded
  print $ initHeight + (heightPerCycle * cyclesNeeded)
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

foo :: [Direction] -> Chamber -> (Chamber, Cache)
foo dirs chamber = runState (progress dirs dirs chamber) ([], 0, 0)

updateCache :: Maybe Int -> Maybe Int
updateCache Nothing = Just 1
updateCache (Just x) = Just (x + 1)

findCycle :: CacheKey -> [CacheKey] -> ([CacheKey], [CacheKey])
findCycle key seq
  | length chunks > n && lastNEqual n = (concat (take (length chunks - n) chunks), key : last chunks)
  | otherwise = (seq, [])
  where
    n = 2
    chunks = init $ splitOn [key] seq
    lastNEqual n = allEqual (drop (length chunks - n) chunks)
    allEqual [] = True
    allEqual (x : xs) = all (== x) xs

progress :: [Direction] -> [Direction] -> Chamber -> State Cache Chamber
progress fullDirList [] chamber = do
  (chamberSequence, prevHeight, prevRockCount) <- get
  let key = (rockShape <$> chamberCurrentRock chamber, currentRockCount - prevRockCount, currentTowerHeight - prevHeight)
  let chamberSequence' = chamberSequence ++ [key]
  put (chamberSequence', currentTowerHeight, currentRockCount)
  let (pre, cycles) = findCycle key chamberSequence'
  if null cycles
    then progress fullDirList fullDirList chamber
    else return chamber
  where
    currentTowerHeight = length (chamberFree chamber)
    currentRockCount = chamberRockCount chamber
progress fullDirList dirs@(direction : rest) chamber@Chamber {..} =
  if chamberRockCount == maxRockCount
    then do
      return chamber
    else case (chamberCurrentRock, direction) of
      (Nothing, _) -> progress fullDirList dirs $ nextRock chamber
      (Just rock, Down) -> progress fullDirList rest $ moveDown chamber rock
      (Just rock, hDir) -> progress fullDirList rest $ moveHorizontally chamber rock hDir

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
