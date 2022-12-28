{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Day16.Part1 where

import Data.List (concatMap, find)
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
  deriving (Show)

data Room = Room
  { roomName :: String,
    roomPressureRate :: Int,
    roomAdjacents :: [String]
  }
  deriving (Show, Eq, Ord)

newtype Cave = Cave
  {caveRooms :: [Room]}
  deriving (Show)

type DistanceMap = M.Map (String, String) Int

run :: IO ()
run = do
  putStrLn "Running Day 16, Part 1 solution..."
  input <- readInputFile
  let c@(Cave rooms) = makeCave input
  let startRoom = fromJust $ getRoom c "AA"
  let initState = CaveState c M.empty 0 startRoom
  print $ maximum $ paths (distMap c) initState
  return ()

readInputFile :: IO String
readInputFile = readFile "aoc/src/Day16/input.txt"

paths :: DistanceMap -> CaveState -> [Int]
paths distanceMap s = dfs
  where
    dfs =
      case possibleNextStates s distanceMap of
        [] -> [pressureReleased s]
        xs -> concatMap (paths distanceMap) xs

pressureReleased :: CaveState -> Int
pressureReleased caveState@CaveState {..} = M.foldlWithKey calc 0 roomsWithOpenValves
  where
    calc pressure room@Room {..} minuteOpened = pressure + (30 - minuteOpened) * roomPressureRate

possibleNextStates :: CaveState -> DistanceMap -> [CaveState]
possibleNextStates CaveState {..} distanceMap = map (\nextRoom -> CaveState cave (openValve nextRoom) (minutesPassed + timeToTurn nextRoom) nextRoom) possibleNextRooms
  where
    possibleNextRooms = filter allConditions (caveRooms cave)
    valvesClosed r = roomName r `notElem` map roomName (M.keys roomsWithOpenValves)
    nonZeroRate r = roomPressureRate r > 0
    timeNeededBelowLimit r = (minutesPassed + timeToTurn r) <= 30
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
    roomPairs = makeRoomPairs rooms
    accum pair@(room1, room2) mapping = M.insert (roomName room1, roomName room2) (distance cave pair) (M.insert (roomName room2, roomName room1) (distance cave pair) mapping)

getAdjacentRooms :: Cave -> Room -> [Room]
getAdjacentRooms cave@(Cave rooms) Room {..} = mapMaybe (getRoom cave) roomAdjacents

makeRoomPairs :: [Room] -> [(Room, Room)]
makeRoomPairs [] = []
makeRoomPairs (x : xs) = pairRooms x xs ++ makeRoomPairs xs
  where
    pairRooms elem [] = []
    pairRooms elem (x : xs) = (elem, x) : pairRooms elem xs
