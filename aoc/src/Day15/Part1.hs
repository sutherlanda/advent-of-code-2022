module Day15.Part1 (run) where

import Data.List.Split (dropDelims, oneOf, split)

data Model = Model Bounds [Sensor]

data Sensor = Sensor Coordinates BeaconCoordinates Radius
  deriving (Show)

type BeaconCoordinates = Coordinates

type Radius = Int

type Bounds = (Coordinates, Coordinates)

type Coordinates = (Int, Int)

run :: IO ()
run = do
  putStrLn "Running Day 15, Part 1 solution..."
  input <- readInputFile
  let sensors = map parseSensorEntry input
  let modelBounds@((xMin, xMax), _) = getModelBounds sensors
  let result = foldl (withinSensorRadius (Model modelBounds sensors)) 0 $ zip [xMin .. xMax] (repeat 2000000)
  print $ "Result: " ++ show result
  return ()

withinSensorRadius :: Model -> Int -> Coordinates -> Int
withinSensorRadius model@(Model bounds sensors) count coords@(x, y) = count + if safeFirst (dropWhile not (scanl checkSensor False sensors)) then 1 else 0
  where
    safeFirst lst = not (null lst) && head lst
    checkSensor _ (Sensor sensorCoords beaconCoords radius) = coords /= beaconCoords && distanceToSensor sensorCoords <= radius
    distanceToSensor sensorCoords@(sX, sY) = abs (x - sX) + abs (y - sY)

readInputFile :: IO [String]
readInputFile = do
  contents <- readFile "aoc/src/Day15/test.txt"
  return (lines contents)

parseSensorEntry :: String -> Sensor
parseSensorEntry line = Sensor sensorCoords beaconCoords radius
  where
    radius = abs (sX - bX) + abs (sY - bY)
    parseLine = filter (not . null) . split (dropDelims $ oneOf ",: ")
    sensorCoords@(sX, sY) = (read $ drop 2 $ parseLine line !! 2, read $ drop 2 $ parseLine line !! 3)
    beaconCoords@(bX, bY) = (read $ drop 2 $ parseLine line !! 8, read $ drop 2 $ parseLine line !! 9)

getModelBounds :: [Sensor] -> Bounds
getModelBounds sensors = ((actualMinX, actualMaxX), (minY, maxY))
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
