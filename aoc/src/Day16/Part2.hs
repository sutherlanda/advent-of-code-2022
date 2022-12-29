{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Day16.Part2 where

import Control.Monad.State
import Data.Graph (dfs)
import Data.List (concatMap, find, intersect, maximumBy)
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust, fromMaybe, mapMaybe)
import qualified Data.Sequence as Seq
import qualified Data.Set as S

data CaveState = CaveState
  { cave :: Cave,
    roomsWithOpenValves :: M.Map Room Int, -- Map of rooms to minute when room valve was opened
    minutesPassed :: Int,
    currentRoom :: Room
  }
  deriving (Show, Eq, Ord)

data Room = Room
  { roomName :: String,
    roomPressureRate :: Int,
    roomAdjacents :: [String]
  }
  deriving (Show, Eq, Ord)

newtype Cave = Cave
  {caveRooms :: [Room]}
  deriving (Show, Eq, Ord)

type DistanceMap = M.Map (String, String) Int

timeLimit = 26 :: Int

run :: IO ()
run = do
  putStrLn "Running Day 16, Part 2 solution..."
  input <- readInputFile
  let c@(Cave rooms) = makeCave input
  let roomMasterSet = S.fromList (map roomName (filter (\Room {..} -> roomPressureRate > 0) rooms))
  let startRoom = fromJust $ getRoom c "AA"
  let initState = CaveState c M.empty 0 startRoom
  let maxPressuresForValveSets = paths (distMap c) initState
  let optimalSingleAgent = maximumBy (\(_, v1) (_, v2) -> v1 `compare` v2) $ M.toList maxPressuresForValveSets
  let remainingValves = S.difference roomMasterSet (fst optimalSingleAgent)
  -- Depending on if the single agent optimal solution would be able to turn all valves, this changes the problem slightly.
  -- If the optimal single agent solution did not turn all valves in the time limit, we need to check the max pressures for aall possible subsets of the remaining valves
  -- If the optimal single agent solution did turn all valves, then we need to compare each optimal valve set with it's complement and take the maximum result.
  if not (S.null remainingValves)
    then do
      let possibleRemainingValveSets = map S.fromList $ subsets $ S.toList remainingValves
      print $ maximum $ map (+ snd optimalSingleAgent) (mapMaybe (`M.lookup` maxPressuresForValveSets) possibleRemainingValveSets)
    else do
      let complementSets = map (S.difference roomMasterSet) (M.keys maxPressuresForValveSets)
      let zippedSets = zip (M.keys maxPressuresForValveSets) complementSets
      print $ maximum $ mapMaybe (\(s1, s2) -> (+) <$> M.lookup s1 maxPressuresForValveSets <*> M.lookup s2 maxPressuresForValveSets) zippedSets

subsets :: [a] -> [[a]]
subsets [] = [[]]
subsets (x : xs) = subsets xs ++ map (x :) (subsets xs)

openValveSet :: CaveState -> S.Set String
openValveSet CaveState {..} = S.fromList $ map roomName $ M.keys roomsWithOpenValves

readInputFile :: IO String
readInputFile = readFile "aoc/src/Day16/input.txt"

paths :: DistanceMap -> CaveState -> M.Map (S.Set String) Int
paths distanceMap st = execState (dfs st) M.empty
  where
    dfs st' =
      let cache Nothing = Just pres
          cache (Just oldPres) = Just (max pres oldPres)
          pres = pressureReleased st'
       in do
            modify $ M.alter cache (openValveSet st')
            let nexts = possibleNextStates st' distanceMap
            mapM_ dfs nexts

pressureReleased :: CaveState -> Int
pressureReleased caveState@CaveState {..} = M.foldlWithKey calc 0 roomsWithOpenValves
  where
    calc pressure room@Room {..} minuteOpened = pressure + (timeLimit - minuteOpened) * roomPressureRate

possibleNextStates :: CaveState -> DistanceMap -> [CaveState]
possibleNextStates CaveState {..} distanceMap = map (\nextRoom -> CaveState cave (openValve nextRoom) (minutesPassed + timeToTurn nextRoom) nextRoom) possibleNextRooms
  where
    possibleNextRooms = filter allConditions (caveRooms cave)
    valvesClosed r = roomName r `notElem` map roomName (M.keys roomsWithOpenValves)
    nonZeroRate r = roomPressureRate r > 0
    timeNeededBelowLimit r = (minutesPassed + timeToTurn r) <= timeLimit
    allConditions r = all ($ r) [nonZeroRate, valvesClosed, timeNeededBelowLimit]
    timeToTurn r = fromMaybe 100 (M.lookup (roomName currentRoom, roomName r) distanceMap) + 1
    openValve r = M.insert r (minutesPassed + timeToTurn r) roomsWithOpenValves

makeCave :: String -> Cave
makeCave text =
  foldr accum (Cave []) $ lines text
  where
    accum line (Cave rooms) =
      let tokens = words line
          name = tokens !! 1
          rate = read (init (drop 5 (tokens !! 4)))
          connected = map (filter (`notElem` ",")) (drop 9 tokens)
       in Cave (Room name rate connected : rooms)

getRoom :: Cave -> String -> Maybe Room
getRoom (Cave rooms) name = find (\Room {..} -> roomName == name) rooms

distance :: Cave -> (Room, Room) -> Int
distance cave@(Cave rooms) (src, dest) = fromMaybe 0 $ bfs (S.singleton src) (Seq.singleton (src, 0)) dest
  where
    bfs :: S.Set Room -> Seq.Seq (Room, Int) -> Room -> Maybe Int
    bfs visited toVisit sought =
      case Seq.viewl toVisit of
        Seq.EmptyL -> Nothing
        (at, dist) Seq.:< rest ->
          let neighbours = filter (`notElem` visited) $ getAdjacentRooms cave at
              visited' = S.union visited $ S.fromList neighbours
              next = Seq.fromList $ map (,dist + 1) neighbours
           in if at == sought
                then Just dist
                else bfs visited' (rest Seq.>< next) sought

-- Create a mapping that contains all the distances between every pair of rooms
distMap :: Cave -> DistanceMap
distMap cave@(Cave rooms) = foldr accum M.empty roomPairs
  where
    roomPairs = makePairs rooms
    accum pair@(room1, room2) mapping = M.insert (roomName room1, roomName room2) (distance cave pair) (M.insert (roomName room2, roomName room1) (distance cave pair) mapping)

getAdjacentRooms :: Cave -> Room -> [Room]
getAdjacentRooms cave@(Cave rooms) Room {..} = mapMaybe (getRoom cave) roomAdjacents

makePairs :: [a] -> [(a, a)]
makePairs [] = []
makePairs (x : xs) = pairRooms x xs ++ makePairs xs
  where
    pairRooms elem [] = []
    pairRooms elem (x : xs) = (elem, x) : pairRooms elem xs
