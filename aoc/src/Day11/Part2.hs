module Day11.Part2 (run) where

import Data.Char (isSpace)
import Data.List (sort, sortBy)
import Data.List.Split (splitOn, splitWhen)
import Debug.Trace (traceShow)

type Index = Int

type Item = Int

type Operation = Int -> Int

type Divisor = Int

type Test = (Int -> Bool, Divisor, PassAction, FailAction)

type PassAction = Int

type FailAction = Int

type InspectionCount = Int

data Monkey = Monkey Index [Item] Operation Test InspectionCount

instance Show Monkey where
  show (Monkey index items op test count) = "Monkey " ++ show index ++ ": " ++ "Items " ++ show items ++ " Count: " ++ show count

run :: IO ()
run = do
  putStrLn "Running Day 11, Part 2 solution..."
  input <- readInputFile
  let groups = splitWhen (null . trim) input
  let monkeys = reverse $ foldl processMonkey [] groups
  let monkeyLCM = foldl1 lcm $ map (\(Monkey _ _ _ (_, divisor, _, _) _) -> divisor) monkeys
  let after20Rounds = last $ take 10001 $ iterate (\m -> foldl (inspect monkeyLCM) m [0 .. length monkeys - 1]) monkeys
  let monkeyActivity = map (\(Monkey _ _ _ _ count) -> count) after20Rounds
  let (mostActive : secondMostActive : _) = reverse $ sort monkeyActivity
  print (mostActive * secondMostActive)

readInputFile :: IO [String]
readInputFile = do
  contents <- readFile "aoc/src/Day11/input.txt"
  return (lines contents)

inspect :: Divisor -> [Monkey] -> Int -> [Monkey]
inspect monkeyLCM monkies monkeyIndex = do
  let postInspectionMonkeys = foldl (inspectItem monkeyLCM) (inspectionMonkey, inspectionPassMonkey, inspectionFailMonkey) itemList
  updatedMonkeyList postInspectionMonkeys
  where
    inspectionMonkey@(Monkey index itemList op (testFn, _, passIndex, failIndex) _) = monkies !! monkeyIndex
    inspectionPassMonkey = monkies !! passIndex
    inspectionFailMonkey = monkies !! failIndex
    tupleToList (x, y, z) = [x, y, z]
    updatedMonkeyList changed = foldl spliceMonkey monkies $ tupleToList changed

inspectItem :: Divisor -> (Monkey, Monkey, Monkey) -> Item -> (Monkey, Monkey, Monkey)
inspectItem monkeyLCM (monkeyA@(Monkey _ [] _ _ _), monkeyB, monkeyC) _ = (monkeyA, monkeyB, monkeyC)
inspectItem monkeyLCM (Monkey indexA (_ : itemListA) opA testA@(testFn, divisor, _, _) countA, Monkey indexB itemListB opB testB@(_, passDivisor, _, _) countB, Monkey indexC itemListC opC testC@(_, failDivisor, _, _) countC) item = do
  let updatedWorryLevel = opA item
  if testFn updatedWorryLevel
    then (Monkey indexA itemListA opA testA (countA + 1), Monkey indexB (itemListB ++ [updatedWorryLevel `mod` monkeyLCM]) opB testB countB, Monkey indexC itemListC opC testC countC)
    else (Monkey indexA itemListA opA testA (countA + 1), Monkey indexB itemListB opB testB countB, Monkey indexC (itemListC ++ [updatedWorryLevel `mod` monkeyLCM]) opC testC countC)

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
parseTest lines = (testFn, divisor, pass, fail)
  where
    divisor = read (last $ words $ head lines)
    testFn = \input -> input `mod` divisor == 0
    pass = read (last $ words $ lines !! 1)
    fail = read (last $ words $ lines !! 2)

trim :: String -> String
trim = f . f
  where
    f = reverse . dropWhile isSpace
