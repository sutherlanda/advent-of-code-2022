module Day18.Part2 where

import Data.List.Split (splitOn)
import qualified Data.Set as S

type Coordinates = (Int, Int, Int)

type Lava = Coordinates

type Water = Coordinates

type AxisBounds = (Int, Int)

type Bounds = (AxisBounds, AxisBounds, AxisBounds)

run :: IO ()
run = do
  putStrLn "Running Day 18, Part 2 solution..."
  input <- readInputFile
  let lava' = S.fromList $ lava input
  let bounds = scanBounds lava'
  let water = pour bounds lava'
  let lavaSurfaceArea = sum $ map (surfaceArea water lava') $ S.toList lava'
  print lavaSurfaceArea
  return ()

offsets :: [Coordinates]
offsets = [(-1, 0, 0), (1, 0, 0), (0, -1, 0), (0, 1, 0), (0, 0, -1), (0, 0, 1)]

readInputFile :: IO String
readInputFile = readFile "aoc/src/Day18/input.txt"

lava :: String -> [Lava]
lava s = map lava' $ words s
  where
    lava' str = toTuple $ map read $ splitOn "," str
    toTuple [x, y, z] = (x, y, z)
    toTuple _ = undefined

pour :: Bounds -> S.Set Lava -> S.Set Water
pour ((xMin, xMax), (yMin, yMax), (zMin, zMax)) lava = loop S.empty (S.singleton (0, 0, 0))
  where
    loop pouredWater newWater
      | S.null newWater = pouredWater
      | otherwise = loop (S.union pouredWater newWater) (addWater pouredWater newWater)
    addWater pouredWater water = foldl (\newWater here -> let freeNeighbours = filter (isFree pouredWater) (neighbours here) in foldl (flip S.insert) newWater freeNeighbours) S.empty water
    tank = [(x, y, z) | x <- [xMin .. xMax], y <- [yMin .. yMax], z <- [zMin .. zMax], not (S.member (x, y, z) lava)]
    isFree water coords = not (S.member coords lava) && not (S.member coords water) && inRange coords
    neighbours coords = map (add coords) offsets
    add (x', y', z') (x'', y'', z'') = (x' + x'', y' + y'', z' + z'')
    inRange (x, y, z) = x >= xMin && x <= xMax && y >= yMin && y <= yMax && z >= zMin && z <= zMax

scanBounds :: S.Set Lava -> Bounds
scanBounds lava = (\((x1, x2), (y1, y2), (z1, z2)) -> ((x1 - 1, x2 + 1), (y1 - 1, y2 + 1), (z1 - 1, z2 + 1))) $ foldl (\((xMin, xMax), (yMin, yMax), (zMin, zMax)) (x, y, z) -> ((min x xMin, max x xMax), (min y yMin, max y yMax), (min z zMin, max z zMax))) ((1, 1), (1, 1), (1, 1)) $ S.toList lava

surfaceArea :: S.Set Coordinates -> S.Set Lava -> Lava -> Int
surfaceArea water lava droplet@(x, y, z) = length (filter (== True) check)
  where
    check = map (isWater . add droplet) offsets
    add (x', y', z') (x'', y'', z'') = (x' + x'', y' + y'', z' + z'')
    isWater droplet' = S.member droplet' water
