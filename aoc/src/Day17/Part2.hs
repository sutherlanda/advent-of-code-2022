{-# LANGUAGE RecordWildCards #-}

module Day17.Part2 where

import Control.Monad.State
import Data.Bifunctor (bimap, first, second)
import Data.List (concatMap, findIndex, intersperse)
import Data.List.Split (splitOn)
import qualified Data.Map as M
import Data.Maybe (fromJust, fromMaybe, isNothing, mapMaybe)
import qualified Data.Sequence as Seq
import Debug.Trace (traceShow)

data Shape = Minus | Plus | BackwardsL | VLine | Block
  deriving (Show, Eq, Ord)

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

type Coordinates = (Int, Int)

type Offset = Coordinates

data Direction = DLeft | DRight | Down
  deriving (Eq, Show)

type WindGustPattern = String

type CacheKey = (Shape, Direction, [Bool], Int)

type Cache = ([CacheKey], Int)

width = 7 :: Int

maxRockCount = 1000000000000 :: Int

run :: IO ()
run = do
  putStrLn "Running Day 17, Part 2 solution..."
  input <- readInputFile
  let chamber = Chamber [] 0 Nothing
  let dirList = intersperse Down (windPattern input) ++ [Down]
  let (result, (seq, _)) = foo dirList chamber
  let cycleResult@(nonCycle, cycle) = findCycle seq
  let initHeight = foldl (\hCount (_, _, _, h) -> hCount + h) 0 nonCycle
  let initRocks = length nonCycle
  let heightPerCycle = foldl (\hCount (_, _, _, h) -> hCount + h) 0 cycle
  let rocksPerCycle = length cycle
  let remainingRocks = maxRockCount - initRocks
  let cyclesNeeded = remainingRocks `div` rocksPerCycle
  let extraRocksNeeded = remainingRocks `mod` rocksPerCycle
  let height' = initHeight + (heightPerCycle * cyclesNeeded)
  let height'' = foldl (\c (_, _, _, h) -> c + h) 0 $ take (extraRocksNeeded + 1) cycle
  print $ height' + height''

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

foo :: [Direction] -> Chamber -> (Chamber, Cache)
foo dirs chamber = runState (progress dirs dirs chamber) ([], 0)

updateCache :: Maybe Int -> Maybe Int
updateCache Nothing = Just 1
updateCache (Just x) = Just (x + 1)

findCycle :: Eq a => [a] -> ([a], [a])
findCycle xxs = fCycle xxs xxs
  where
    fCycle (x : xs) (_ : y : ys)
      | x == y = fStart xxs xs
      | otherwise = fCycle xs ys
    fCycle _ _ = (xxs, []) -- not cyclic
    fStart (x : xs) (y : ys)
      | x == y = ([], x : fLength x xs)
      | otherwise = let (as, bs) = fStart xs ys in (x : as, bs)
    fStart _ _ = (xxs, [])
    fLength x (y : ys)
      | x == y = []
      | otherwise = y : fLength x ys
    fLength x [] = []

progress :: [Direction] -> [Direction] -> Chamber -> State Cache Chamber
progress fullDirList [] chamber = progress fullDirList fullDirList chamber
progress fullDirList dirs@(direction : rest) chamber =
  if chamberRockCount chamber == maxRockCount
    then do
      return chamber
    else case (chamberCurrentRock chamber, direction) of
      (Nothing, _) -> do
        (sequence, prevHeight) <- get
        let newHeight = length (chamberFree chamber)
        let chamber' = nextRock chamber
        let rock@Rock {..} = fromJust $ chamberCurrentRock chamber'
        let key = (rockShape, direction, concat (take 10 (chamberFree chamber)), newHeight - prevHeight)
        let sequence' = sequence ++ [key]
        put (sequence', newHeight)
        let (_, cycles) = findCycle sequence'
        if null cycles
          then progress fullDirList dirs chamber'
          else return chamber'
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

replaceNth :: Int -> a -> [a] -> [a]
replaceNth idx val lst = take idx lst ++ [val] ++ drop (idx + 1) lst
