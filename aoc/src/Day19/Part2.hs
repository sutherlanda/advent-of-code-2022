{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Day19.Part2 where

import Control.Monad.State
import Data.Bifunctor (bimap)
import Data.Char (isSpace)
import Data.List.Split (dropDelims, oneOf, split, splitOn)
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import qualified Data.Set as S
import Debug.Trace

-- Represents a count of each type of resource
data Resources = Resources
  { resourcesOreCount :: Int,
    resourcesClayCount :: Int,
    resourcesObsidianCount :: Int,
    resourcesGeodeCount :: Int
  }
  deriving (Show, Eq, Ord)

-- Represents a count of each type of robot
data Robots = Robots
  { robotsOreMiners :: Int,
    robotsClayMiners :: Int,
    robotsObsidianMiners :: Int,
    robotsGeodeMiners :: Int
  }
  deriving (Show, Eq, Ord)

-- Represents a blueprint enumerating the cost of each type of robot
data Blueprint = Blueprint
  { blueprintId :: Int,
    blueprintOreRobot :: Cost,
    blueprintClayRobot :: Cost,
    blueprintObsidianRobot :: Cost,
    blueprintGeodeRobot :: Cost
  }
  deriving (Show, Eq, Ord)

-- Represents an expense breakdown to build a robot
data Cost = Cost
  { priceOre :: Int,
    priceClay :: Int,
    priceObsidian :: Int
  }
  deriving (Show, Eq, Ord)

-- A state representing a combination of resource and robot count along with minutes passed
data State' = State'
  { stateBlueprint :: Blueprint,
    stateResources :: Resources,
    stateRobots :: Robots,
    stateMinutesPassed :: Int
  }
  deriving (Show, Eq, Ord)

data Robot = OreRobot | ClayRobot | ObsidianRobot | GeodeRobot
  deriving (Show, Eq, Ord)

timeLimit = 32 :: Int

run :: IO ()
run = do
  putStrLn "Running Day 19, Part 2 solution..."
  input <- readInputFile
  let blueprints' = blueprints input
  print $ product $ map maxGeodes $ take 3 blueprints'
  return ()

readInputFile :: IO String
readInputFile = readFile "aoc/src/Day19/input.txt"

maxGeodes :: Blueprint -> Int
maxGeodes bp@Blueprint {..} = decisionMap
  where
    initialState = State' bp (Resources 0 0 0 0) (Robots 1 0 0 0) 0
    decisionMap = decisions initialState

mine :: State' -> Int -> State'
mine s@State' {..} minutes = s {stateResources = nextResources stateResources stateRobots, stateMinutesPassed = stateMinutesPassed + minutes}
  where
    nextResources Resources {..} Robots {..} =
      Resources
        { resourcesOreCount = resourcesOreCount + (minutes * robotsOreMiners),
          resourcesClayCount = resourcesClayCount + (minutes * robotsClayMiners),
          resourcesObsidianCount = resourcesObsidianCount + (minutes * robotsObsidianMiners),
          resourcesGeodeCount = resourcesGeodeCount + (minutes * robotsGeodeMiners)
        }

build :: State' -> Robot -> State'
build s robot = postMineState {stateResources = postBuildResources, stateRobots = postBuildRobots}
  where
    postMineState = mine s (miningTime s + 1)
    postMineStateResources = stateResources postMineState
    bp = stateBlueprint s
    robots = stateRobots s

    (postBuildResources, postBuildRobots) =
      case robot of
        OreRobot -> (postMineStateResources {resourcesOreCount = resourcesOreCount postMineStateResources - priceOre (blueprintOreRobot bp)}, robots {robotsOreMiners = robotsOreMiners robots + 1})
        ClayRobot -> (postMineStateResources {resourcesOreCount = resourcesOreCount postMineStateResources - priceOre (blueprintClayRobot bp)}, robots {robotsClayMiners = robotsClayMiners robots + 1})
        ObsidianRobot -> (postMineStateResources {resourcesOreCount = resourcesOreCount postMineStateResources - priceOre (blueprintObsidianRobot bp), resourcesClayCount = resourcesClayCount postMineStateResources - priceClay (blueprintObsidianRobot bp)}, robots {robotsObsidianMiners = robotsObsidianMiners robots + 1})
        GeodeRobot -> (postMineStateResources {resourcesOreCount = resourcesOreCount postMineStateResources - priceOre (blueprintGeodeRobot bp), resourcesObsidianCount = resourcesObsidianCount postMineStateResources - priceObsidian (blueprintGeodeRobot bp)}, robots {robotsGeodeMiners = robotsGeodeMiners robots + 1})
    -- Returns the time needed to mine enough materials to build the specified robot
    miningTime State' {..} =
      case robot of
        OreRobot -> minutesForOre (blueprintOreRobot bp)
        ClayRobot -> minutesForOre (blueprintClayRobot bp)
        ObsidianRobot -> max (minutesForOre (blueprintObsidianRobot bp)) (minutesForClay (blueprintObsidianRobot bp))
        GeodeRobot -> max (minutesForOre (blueprintGeodeRobot bp)) (minutesForObsidian (blueprintGeodeRobot bp))
      where
        minutesForOre robotCost =
          if resourcesOreCount stateResources >= priceOre robotCost
            then 0
            else ceiling (fromIntegral (priceOre robotCost - resourcesOreCount stateResources) / fromIntegral (robotsOreMiners stateRobots))
        minutesForClay robotCost =
          if resourcesClayCount stateResources >= priceClay robotCost
            then 0
            else ceiling (fromIntegral (priceClay robotCost - resourcesClayCount stateResources) / fromIntegral (robotsClayMiners stateRobots))
        minutesForObsidian robotCost =
          if resourcesObsidianCount stateResources >= priceObsidian robotCost
            then 0
            else ceiling (fromIntegral (priceObsidian robotCost - resourcesObsidianCount stateResources) / fromIntegral (robotsObsidianMiners stateRobots))

-- Given a state and a decision for next robot to build, determine the next state by updating resources, robots, and minutes passed
nextState :: State' -> Robot -> Maybe State'
nextState s robot =
  if stateMinutesPassed stateAfterBuilding' > timeLimit
    then Nothing
    else Just stateAfterBuilding'
  where
    stateAfterBuilding' = build s robot

-- Given a state with a robot configuration, determine the set of possible robots to build next
possibleNextRobotBuilds :: State' -> S.Set Robot
possibleNextRobotBuilds State' {..} = S.fromList $ filter filterRobots [OreRobot, ClayRobot, ObsidianRobot, GeodeRobot]
  where
    maxOreCost = maximum (map priceOre [blueprintOreRobot stateBlueprint, blueprintClayRobot stateBlueprint, blueprintObsidianRobot stateBlueprint, blueprintGeodeRobot stateBlueprint])
    maxClayCost = maximum (map priceClay [blueprintOreRobot stateBlueprint, blueprintClayRobot stateBlueprint, blueprintObsidianRobot stateBlueprint, blueprintGeodeRobot stateBlueprint])
    maxObsidianCost = maximum (map priceObsidian [blueprintOreRobot stateBlueprint, blueprintClayRobot stateBlueprint, blueprintObsidianRobot stateBlueprint, blueprintGeodeRobot stateBlueprint])
    atMaxOreRobots = robotsOreMiners stateRobots >= maxOreCost
    atMaxClayRobots = robotsClayMiners stateRobots >= maxClayCost
    atMaxObsidianRobots = robotsObsidianMiners stateRobots >= maxObsidianCost
    canBuildObsidianRobot = not atMaxObsidianRobots && robotsClayMiners stateRobots > 0
    canBuildClayRobot = not atMaxClayRobots
    canBuildGeodeRobot = robotsObsidianMiners stateRobots > 0
    canBuildOreRobot = not atMaxOreRobots
    filterRobots OreRobot = canBuildOreRobot
    filterRobots ClayRobot = canBuildClayRobot
    filterRobots ObsidianRobot = canBuildObsidianRobot
    filterRobots GeodeRobot = canBuildGeodeRobot

decisions :: State' -> Int
decisions s = execState (loop s) 0
  where
    loop s' = do
      -- Get possible 'next' states
      let nexts = mapMaybe (nextState s') (S.toList (possibleNextRobotBuilds s'))
      -- If there are none, then we're at time limit, and see if theere is a new max and save
      when (null nexts) $ modify (\m -> max m (resourcesGeodeCount (stateResources s')))
      -- Only continue checking paths that could possibly result in a new max
      currentBest <- get
      let filteredNexts = filter (\nextS -> currentBest < estimateGeodePayoff nextS) nexts
      mapM_ loop filteredNexts

estimateGeodePayoff :: State' -> Int
estimateGeodePayoff State' {..} = resourcesGeodeCount stateResources + sum (map (\n -> robotsGeodeMiners stateRobots + (timeLeft - n)) [0 .. timeLeft])
  where
    timeLeft = timeLimit - stateMinutesPassed

blueprints :: String -> [Blueprint]
blueprints = map blueprint . lines
  where
    blueprint s = Blueprint (blueprintId (tokens s)) (oreCost 1 s) (oreCost 2 s) (oreAndClayCost 3 s) (oreAndObsCost 4 s)
    tokens = map trim . split (dropDelims (oneOf ":."))
    blueprintId = read . last . words . (!! 0)
    oreCost n = (\c -> Cost c 0 0) . singleCost n . tokens
    oreAndClayCost n = (\(oc, cc) -> Cost oc cc 0) . multiCost n . tokens
    oreAndObsCost n = (\(oc, obc) -> Cost oc 0 obc) . multiCost n . tokens
    singleCost n = read . (!! 4) . words . (!! n)
    multiCost n = bimap (read . (!! 4)) (read . (!! 7)) . tuplify . words . (!! n)
    tuplify a = (a, a)

trim :: String -> String
trim = f . f
  where
    f = reverse . dropWhile isSpace
