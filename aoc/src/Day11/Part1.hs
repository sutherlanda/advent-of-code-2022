module Day11.Part1 (run) where

import Data.Char (isSpace)
import Data.List (sort, sortBy)
import Data.List.Split (splitOn, splitWhen)

type Index = Int

type Item = Int

type Operation = Int -> Int

type Test = (Int -> Bool, PassAction, FailAction)

type PassAction = Int

type FailAction = Int

type InspectionCount = Int

data Monkey = Monkey Index [Item] Operation Test InspectionCount

instance Show Monkey where
  show (Monkey index items op test count) = "Monkey " ++ show index ++ ": " ++ "Items " ++ show items ++ " Count: " ++ show count

run :: IO ()
run = do
  putStrLn "Running Day 11, Part 1 solution..."
  input <- readInputFile
  let groups = splitWhen (null . trim) input
  let monkeys = reverse $ foldl processMonkey [] groups
  let after20Rounds = last $ take 21 $ iterate (\m -> foldl inspect m [0 .. length monkeys - 1]) monkeys
  let monkeyActivity = map (\(Monkey _ _ _ _ count) -> count) after20Rounds
  let (mostActive : secondMostActive : _) = reverse $ sort monkeyActivity
  print (mostActive * secondMostActive)

readInputFile :: IO [String]
readInputFile = do
  contents <- readFile "aoc/src/Day11/input.txt"
  return (lines contents)

inspect :: [Monkey] -> Int -> [Monkey]
inspect monkies monkeyIndex = do
  let postInspectionMonkeys = foldl inspectItem (inspectionMonkey, inspectionPassMonkey, inspectionFailMonkey) itemList
  updatedMonkeyList postInspectionMonkeys
  where
    inspectionMonkey@(Monkey index itemList op (testFn, passIndex, failIndex) _) = monkies !! monkeyIndex
    inspectionPassMonkey = monkies !! passIndex
    inspectionFailMonkey = monkies !! failIndex
    tupleToList (x, y, z) = [x, y, z]
    updatedMonkeyList changed = foldl spliceMonkey monkies $ tupleToList changed

inspectItem :: (Monkey, Monkey, Monkey) -> Item -> (Monkey, Monkey, Monkey)
inspectItem (monkeyA@(Monkey _ [] _ _ _), monkeyB, monkeyC) _ = (monkeyA, monkeyB, monkeyC)
inspectItem (Monkey indexA (_ : itemListA) opA testA@(testFn, _, _) countA, Monkey indexB itemListB opB testB countB, Monkey indexC itemListC opC testC countC) item = do
  let updatedWorryLevel = opA item
  let boredWorryLevel = updatedWorryLevel `div` 3
  if testFn boredWorryLevel
    then (Monkey indexA itemListA opA testA (countA + 1), Monkey indexB (itemListB ++ [boredWorryLevel]) opB testB countB, Monkey indexC itemListC opC testC countC)
    else (Monkey indexA itemListA opA testA (countA + 1), Monkey indexB itemListB opB testB countB, Monkey indexC (itemListC ++ [boredWorryLevel]) opC testC countC)

spliceMonkey :: [Monkey] -> Monkey -> [Monkey]
spliceMonkey monkeys monkey@(Monkey index _ _ _ _) = take index monkeys ++ [monkey] ++ drop (index + 1) monkeys

processMonkey :: [Monkey] -> [String] -> [Monkey]
processMonkey monkeyList lines = parse (length monkeyList) : monkeyList
  where
    parse index = do
      let itemList = map read $ splitOn ", " $ trim $ last $ splitOn ":" (lines !! 1)
      let operation = parseOperation $ lines !! 2
      let test = parseTest $ drop 3 lines
      Monkey index itemList operation test 0

parseOperation :: String -> Operation
parseOperation line = op
  where
    tokens = words line
    op =
      case (tokens !! 4, tokens !! 5) of
        ("*", "old") -> (\input -> input * input)
        ("*", o) -> (\input -> input * read o)
        ("+", o) -> (\input -> input + read o)
        _ -> id

parseTest :: [String] -> Test
parseTest lines = (testFn, pass, fail)
  where
    testFn = \input -> input `mod` read (last $ words $ head lines) == 0
    pass = read (last $ words $ lines !! 1)
    fail = read (last $ words $ lines !! 2)

trim :: String -> String
trim = f . f
  where
    f = reverse . dropWhile isSpace
