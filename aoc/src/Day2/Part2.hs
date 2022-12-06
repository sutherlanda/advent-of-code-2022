module Day2.Part2 (run) where

import Data.Maybe (fromJust)

data Shape = Rock | Paper | Scissors

data Outcome = Win | Loss | Tie

run :: IO ()
run = do
  putStrLn "Running Day 2, Part 2 solution..."
  strategyGuide <- readInputFile
  let score = sum $ map decodeAndScore strategyGuide
  putStr "The total score is "
  print score
  return ()

decodeAndScore :: String -> Int
decodeAndScore codeString = shapeValue + outcomeValue
  where
    firstShape = fromJust $ decodeToShape $ head codeString
    desiredOutcome = fromJust $ decodeToNeededOutcome $ last codeString
    secondShape = getShapeForNeededOutcome firstShape desiredOutcome
    shapeValue = getShapeValue secondShape
    outcomeValue = getOutcomeValue desiredOutcome

readInputFile :: IO [String]
readInputFile = do
  contents <- readFile "aoc/src/Day2/input.txt"
  return (lines contents)

decodeToShape :: Char -> Maybe Shape
decodeToShape code =
  case code of
    'A' -> Just Rock
    'B' -> Just Paper
    'C' -> Just Scissors
    _ -> Nothing

decodeToNeededOutcome :: Char -> Maybe Outcome
decodeToNeededOutcome code =
  case code of
    'X' -> Just Loss
    'Y' -> Just Tie
    'Z' -> Just Win
    _ -> Nothing

getShapeForNeededOutcome :: Shape -> Outcome -> Shape
getShapeForNeededOutcome shape outcome =
  case (shape, outcome) of
    (Rock, Win) -> Paper
    (Rock, Tie) -> Rock
    (Rock, Loss) -> Scissors
    (Paper, Win) -> Scissors
    (Paper, Tie) -> Paper
    (Paper, Loss) -> Rock
    (Scissors, Win) -> Rock
    (Scissors, Tie) -> Scissors
    (Scissors, Loss) -> Paper

getOutcome :: Shape -> Shape -> Outcome
getOutcome opponent mine =
  case (opponent, mine) of
    (Rock, Rock) -> Tie
    (Rock, Paper) -> Win
    (Rock, Scissors) -> Loss
    (Paper, Paper) -> Tie
    (Paper, Rock) -> Loss
    (Paper, Scissors) -> Win
    (Scissors, Scissors) -> Tie
    (Scissors, Rock) -> Win
    (Scissors, Paper) -> Loss

getShapeValue :: Shape -> Int
getShapeValue shape =
  case shape of
    Rock -> 1
    Paper -> 2
    Scissors -> 3

getOutcomeValue :: Outcome -> Int
getOutcomeValue outcome =
  case outcome of
    Win -> 6
    Tie -> 3
    Loss -> 0
