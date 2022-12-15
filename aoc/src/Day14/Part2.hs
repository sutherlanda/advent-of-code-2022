module Day14.Part2 (run) where

import Data.List (elemIndex, intercalate, nub)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)
import Debug.Trace (traceShow)

data Material = Rock | Air | Sand | Source
  deriving (Eq)

data Model = Model ModelBounds [[Material]]
  deriving (Eq)

type Distance = Int

type Coordinates = (Int, Int)

type Path = (Distance, Distance)

type RockStructure = [Path]

type ModelBounds = (Coordinates, Coordinates)

data Availability = OutOfBounds | Free | Blocked
  deriving (Eq, Show)

data MovementOutcome = Moved Coordinates | Abyss
  deriving (Eq, Show)

instance Show Material where
  show mat =
    case mat of
      Rock -> "#"
      Air -> "."
      Sand -> "o"
      Source -> "+"

run :: IO ()
run = do
  putStrLn "Running Day 14, Part 1 solution..."
  input <- readInputFile
  let structures = map parseRockStructure input
  let rockCoords = nub $ concatMap expandPaths structures
  let modelBounds = getModelBounds rockCoords
  let model = updateModel Source (initModel modelBounds) (500, 0)
  let modelWithRocks = foldl (updateModel Rock) model rockCoords
  let modelWithFloor = addFloor modelWithRocks
  let completeModel = dropUntilAbyss modelWithFloor
  putStrLn $ "This may take awhile..."
  putStrLn $ "Amount of sand: " ++ show (countSand completeModel)

readInputFile :: IO [String]
readInputFile = do
  contents <- readFile "aoc/src/Day14/input.txt"
  return (lines contents)

addFloor :: Model -> Model
addFloor model@(Model bound@((xMin, xMax), (yMin, yMax)) mats) = do
  let floorCoords = nub $ concatMap expandPaths [[(xMin, yMax), (xMax, yMax)]]
  foldl (updateModel Rock) model floorCoords

countSand :: Model -> Int
countSand (Model _ mats) = length $ filter (== Sand) $ concat mats

initModel :: ModelBounds -> Model
initModel bounds@((minX, maxX), (minY, maxY)) = Model bounds $ replicate (maxY - minY + 1) (replicate (maxX - minX + 1) Air)

printModel :: Model -> IO ()
printModel (Model bounds materials) = mapM_ (putStrLn . materialLineText) materials
  where
    materialLineText = intercalate "" . map show

getModelBounds :: [Coordinates] -> (Coordinates, Coordinates)
getModelBounds rockCoords = ((minX, maxX), (0, maxY))
  where
    minX = minimum (map fst rockCoords) - maxY
    maxX = maximum (map fst rockCoords) + maxY
    maxY = maximum (map snd rockCoords) + 2

updateModel :: Material -> Model -> Coordinates -> Model
updateModel mat (Model bounds@((xMin, _), _) materials) coords@(x, y) = Model bounds $ take y materials ++ [updatedLine] ++ drop (y + 1) materials
  where
    convertedX = x - xMin
    originalLine = materials !! y
    updatedLine = take convertedX originalLine ++ [mat] ++ drop (convertedX + 1) originalLine

parseRockStructure :: String -> RockStructure
parseRockStructure str = foldr parsePath [] $ splitOn " -> " str
  where
    parsePath s structure = tuplify2 (map read (splitOn "," s)) : structure
    tuplify2 [x, y] = (x, y)
    tuplify2 _ = undefined

expandPaths :: RockStructure -> [Coordinates]
expandPaths = fst . foldl expand ([], Nothing)
  where
    expand (coords, maybePrev) nextPoint = maybe ([], Just nextPoint) (\prev -> (coords ++ extrapolate prev nextPoint, Just nextPoint)) maybePrev
    extrapolate prev@(pX, pY) next@(nX, nY) = [(x, y) | x <- [min pX nX .. max pX nX], y <- [min pY nY .. max pY nY]]

dropUntilAbyss :: Model -> Model
dropUntilAbyss model = do
  let updatedModel = dropSand model
  if model == updatedModel
    then updatedModel
    else dropUntilAbyss updatedModel

dropSand :: Model -> Model
dropSand model@(Model bounds@((xMin, _), _) materials) = getNext sourceCoords
  where
    getNext coords = do
      let outcome = nextSandPosition' model coords
      case outcome of
        Abyss -> model
        Moved newCoords -> updateModel Sand model newCoords
    sourceCoords = (500, 0)

nextSandPosition' :: Model -> Coordinates -> MovementOutcome
nextSandPosition' (Model bounds@((xMin, _), (_, yMax)) mats) currentPosition@(x, y) = move currentPosition
  where
    down coords@(x', y') = (x', y' + 1)
    downLeft coords@(x', y') = (x' - 1, y' + 1)
    downRight coords@(x', y') = (x' + 1, y' + 1)
    check coords@(x', y')
      | x' - xMin < 0 || y' > yMax = OutOfBounds
      | otherwise =
        case mats !! y' !! (x' - xMin) of
          Rock -> Blocked
          Sand -> Blocked
          Source -> Blocked
          Air -> Free
    move currentCoords =
      case check (down currentCoords) of
        OutOfBounds -> Abyss
        Free -> move (down currentCoords)
        Blocked ->
          case check (downLeft currentCoords) of
            OutOfBounds -> Abyss
            Free -> move (downLeft currentCoords)
            Blocked ->
              case check (downRight currentCoords) of
                OutOfBounds -> Abyss
                Free -> move (downRight currentCoords)
                Blocked -> Moved currentCoords
