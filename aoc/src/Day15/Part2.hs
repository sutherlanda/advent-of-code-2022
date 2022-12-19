module Day15.Part2 (run) where

import Data.Bifunctor (bimap)
import Data.List (find, nub)
import Data.List.Split (dropDelims, oneOf, split)
import Data.Maybe (listToMaybe, mapMaybe)

data Model = Model Bounds [Sensor]

data Sensor = Sensor Coordinates BeaconCoordinates Radius
  deriving (Show)

type BeaconCoordinates = Coordinates

type Radius = Int

type Bounds = (Coordinates, Coordinates)

type Coordinates = (Int, Int)

type BoundLimits = Bounds

type Line = (Coordinates, Coordinates)

run :: IO ()
run = do
  putStrLn "Running Day 15, Part 2 solution..."
  input <- readInputFile
  let limits = ((0, 4000000), (0, 4000000))
  let sensors = map parseSensorEntry input
  let modelBounds@((xMin, xMax), _) = getModelBounds limits sensors
  -- We check coordinates adjacent to an intersection as these are the only possible points where a undetected beacon may exist
  let intersections = getIntersectionPoints modelBounds sensors
  let (x, y) = head $ nub $ mapMaybe (checkNeighbours modelBounds sensors) intersections
  print $ x * 4000000 + y

outsideRadius :: Model -> Int -> Coordinates -> Int
outsideRadius model@(Model bounds sensors) count coords@(x, y) =
  count + if safeFirst (dropWhile (== True) (scanl checkSensor True sensors)) then 1 else 0
  where
    safeFirst lst = null lst || head lst
    checkSensor _ (Sensor sensorCoords beaconCoords radius) =
      coords /= beaconCoords && coords /= sensorCoords && distanceToSensor sensorCoords > radius
    distanceToSensor sensorCoords@(sX, sY) = abs (x - sX) + abs (y - sY)

-- Check neighbouring coordinates to input, and return any coordinates that are not covered by a sensor
checkNeighbours :: Bounds -> [Sensor] -> Coordinates -> Maybe Coordinates
checkNeighbours bounds sensors coords@(x, y) = find (isCoordinateWithinRadius bounds sensors) [north, east, south, west]
  where
    north = (x, y - 1)
    east = (x + 1, y)
    south = (x, y + 1)
    west = (x - 1, y)

isCoordinateWithinRadius :: Bounds -> [Sensor] -> Coordinates -> Bool
isCoordinateWithinRadius bounds@((xMin, xMax), (yMin, yMax)) sensors coords@(x, y) =
  not (x < xMin || x > xMax || y < yMin || y > yMax) && all check sensors
  where
    safeFirst lst = not (null lst) && head lst
    check (Sensor sensorCoords _ radius) = distanceToSensor sensorCoords > radius
    distanceToSensor sensorCoords@(sX, sY) = abs (x - sX) + abs (y - sY)

readInputFile :: IO [String]
readInputFile = do
  contents <- readFile "aoc/src/Day15/input.txt"
  return (lines contents)

parseSensorEntry :: String -> Sensor
parseSensorEntry line = Sensor sensorCoords beaconCoords radius
  where
    radius = abs (sX - bX) + abs (sY - bY)
    parseLine = filter (not . null) . split (dropDelims $ oneOf ",: ")
    sensorCoords@(sX, sY) = (read $ drop 2 $ parseLine line !! 2, read $ drop 2 $ parseLine line !! 3)
    beaconCoords@(bX, bY) = (read $ drop 2 $ parseLine line !! 8, read $ drop 2 $ parseLine line !! 9)

getModelBounds :: BoundLimits -> [Sensor] -> Bounds
getModelBounds limits@((limitXMin, limitXMax), (limitYMin, limitYMax)) sensors = ((limitedMinX, limitedMaxX), (limitedMinY, limitedMaxY))
  where
    extractedBeaconCoords = map (\(Sensor _ beaconCoords _) -> beaconCoords) sensors
    extractedSensorCoords = map (\(Sensor sensorCoords _ _) -> sensorCoords) sensors
    extractedRadii = map (\(Sensor _ _ radius) -> radius) sensors
    minX = minimum (map fst (extractedBeaconCoords ++ extractedSensorCoords))
    maxX = maximum (map fst (extractedBeaconCoords ++ extractedSensorCoords))
    minY = minimum (map snd (extractedBeaconCoords ++ extractedSensorCoords))
    maxY = maximum (map snd (extractedBeaconCoords ++ extractedSensorCoords))

    -- If any of the sensors have a radius that extends beyond the bounds calculated above, that needs to be factored in
    actualMinX = minimum (minX : map (\(Sensor sensorCoords@(x, y) _ radius) -> x - radius) sensors)
    actualMaxX = maximum (maxX : map (\(Sensor sensorCoords@(x, y) _ radius) -> x + radius) sensors)

    -- Part 2: Limit the model boundaries to the supplied limits
    limitedMinX = max actualMinX limitXMin
    limitedMaxX = min actualMaxX limitXMax

    limitedMinY = max minY limitYMin
    limitedMaxY = min maxY limitYMax

getIntersectionPoints :: Bounds -> [Sensor] -> [Coordinates]
getIntersectionPoints bounds@((xMin, xMax), (yMin, yMax)) sensors =
  nub $ mapMaybe (uncurry findIntersection) $ makePairs $ sensorBoundaries ++ outerBoundaries
  where
    outerBoundaries = [((xMin, yMin), (xMax, yMin)), ((xMax, yMin), (xMax, yMax)), ((xMax, yMax), (xMin, yMax)), ((xMin, yMax), (xMin, yMin))]
    sensorBoundaries = getSensorBoundaries sensors
    pairElements elem list =
      case list of
        [] -> []
        x : xs -> (elem, x) : pairElements elem xs
    makePairs list =
      case list of
        [] -> []
        x : xs -> pairElements x xs ++ makePairs xs

findIntersection :: Line -> Line -> Maybe Coordinates
findIntersection line1@((x1, y1), (x2, y2)) line2@((x3, y3), (x4, y4)) =
  if denom == 0 || ua < 0 || ua > 1 || ub < 0 || ub > 1
    then Nothing
    else Just (round (fromIntegral x1 + ua * fromIntegral (x2 - x1)), round (fromIntegral y1 + ua * fromIntegral (y2 - y1)))
  where
    denom = (y4 - y3) * (x2 - x1) - (x4 - x3) * (y2 - y1)
    ua :: Double
    ua = fromIntegral ((x4 - x3) * (y1 - y3) - (y4 - y3) * (x1 - x3)) / fromIntegral denom
    ub = fromIntegral ((x2 - x1) * (y1 - y2) - (y2 - y1) * (x1 - x3)) / fromIntegral denom

getSensorBoundaries :: [Sensor] -> [Line]
getSensorBoundaries = concatMap getSensorBoundary

getSensorBoundary :: Sensor -> [Line]
getSensorBoundary (Sensor sensorCoordinates@(x, y) _ radius) = [(t, r), (r, b), (b, l), (l, t)]
  where
    t = (x, y - radius)
    r = (x + radius, y)
    b = (x, y + radius)
    l = (x - radius, y)
