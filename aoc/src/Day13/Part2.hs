module Day13.Part2 (run) where

import Data.Char (isSpace)
import Data.List (elemIndex, elemIndices, sortBy)
import Data.List.Split (keepDelimsL, oneOf, split, splitWhen)
import Data.Maybe (fromJust, mapMaybe)

type Item = Int

data PacketContent = Single Item | Multiple [PacketContent]
  deriving (Show, Eq)

type Packet = [PacketContent]

type Pair = (Packet, Packet)

data ComparisonResult = InOrder | NotInOrder | Undetermined
  deriving (Eq, Show)

data Zipped = Valid PacketContent | Invalid

run :: IO ()
run = do
  putStrLn "Running Day 13, Part 2 solution..."
  input <- readInputFile
  let strPairs = splitWhen (null . trim) input
  let pairs = mapMaybe parsePair strPairs
  let packetList = flattenTuples pairs ++ [dividerPacket1, dividerPacket2]
  let orderedPacketList = sortBy sortPacketFn packetList
  print $ (fromJust (elemIndex dividerPacket1 orderedPacketList) + 1) * (fromJust (elemIndex dividerPacket2 orderedPacketList) + 1)
  where
    dividerPacket1 = parsePacket "[[2]]"
    dividerPacket2 = parsePacket "[[6]]"

sortPacketFn :: Packet -> Packet -> Ordering
sortPacketFn packet1 packet2 = if compareResult then LT else GT
  where
    compareResult = arePacketsInOrder (packet1, packet2)

flattenTuples :: [(a, a)] -> [a]
flattenTuples ((x1, x2) : xs) = x1 : x2 : flattenTuples xs
flattenTuples [] = []

readInputFile :: IO [String]
readInputFile = do
  contents <- readFile "aoc/src/Day13/input.txt"
  return (lines contents)

arePacketsInOrder :: Pair -> Bool
arePacketsInOrder (left, right) = inOrder $ safeFirst $ dropWhile (== Undetermined) $ scanl comparePacketContent Undetermined (zipEvenly left right)
  where
    inOrder r = r == InOrder || r == Undetermined
    safeFirst lst = if null lst then Undetermined else head lst
    comparePacketContent result content =
      case content of
        (Just (Single l), Just (Single r)) -> compare l r
        (Just (Multiple l), Just (Multiple r)) -> safeFirst $ dropWhile (== Undetermined) $ scanl comparePacketContent Undetermined (zipEvenly l r)
        (Just (Single l), Just (Multiple r)) -> safeFirst $ dropWhile (== Undetermined) $ scanl comparePacketContent Undetermined (zipEvenly [Multiple [Single l]] [Multiple r])
        (Just (Multiple l), Just (Single r)) -> safeFirst $ dropWhile (== Undetermined) $ scanl comparePacketContent Undetermined (zipEvenly [Multiple l] [Multiple [Single r]])
        (Nothing, Just _) -> InOrder
        (Just _, Nothing) -> NotInOrder
        (Nothing, Nothing) -> Undetermined
    compare l r
      | l < r = InOrder
      | l > r = NotInOrder
      | otherwise = Undetermined

zipEvenly :: [PacketContent] -> [PacketContent] -> [(Maybe PacketContent, Maybe PacketContent)]
zipEvenly (x : xs) (y : ys) = (Just x, Just y) : zipEvenly xs ys
zipEvenly [] ys = zip (repeat Nothing) (map Just ys)
zipEvenly xs [] = zip (map Just xs) (repeat Nothing)

parsePair :: [String] -> Maybe Pair
parsePair [str1, str2] = Just (parsePacket str1, parsePacket str2)
parsePair _ = Nothing

parsePacket :: String -> Packet
parsePacket str = x
  where
    splitInput = filter (not . null) $ split (oneOf "[],") str
    (x, _) = foldr folder ([], []) splitInput
    folder :: String -> ([PacketContent], [[PacketContent]]) -> ([PacketContent], [[PacketContent]])
    folder c (content, listStack) =
      case c of
        "]" -> (content, [] : listStack)
        "[" ->
          case listStack of
            [] -> undefined -- Shouldn't happen
            [x] -> (Multiple x : content, [])
            (x : parent : xs) -> (content, (Multiple x : parent) : xs)
        "," -> (content, listStack)
        i ->
          case listStack of
            [] -> (Single (read i) : content, listStack)
            (x : xs) -> (content, (Single (read i) : x) : xs)

trim :: String -> String
trim = f . f
  where
    f = reverse . dropWhile isSpace
