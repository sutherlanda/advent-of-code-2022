module Day7.Part2 (run) where

import Data.HashMap.Internal.Strict (size)
import Data.List (find, isPrefixOf)
import Data.Maybe (fromJust)

type Name = String

type Size = Int

data Directory = Directory Name Size

data File = File Name Size

instance Show Directory where
  show (Directory name size) = "Dir " ++ name ++ " " ++ show size

run :: IO ()
run = do
  putStrLn "Running Day 7, Part 2 solution..."
  input <- readInputFile
  let (_, sizes) = emptyStack $ foldl folder ([], []) input
  let unusedSpace = 70000000 - maximum sizes
  let spaceToDelete = 30000000 - unusedSpace
  let smallestCandidate = minimum $ filter (>= spaceToDelete) sizes
  print smallestCandidate
  return ()

readInputFile :: IO [String]
readInputFile = do
  contents <- readFile "aoc/src/Day7/input.txt"
  return (lines contents)

folder :: ([Directory], [Int]) -> String -> ([Directory], [Int])
folder accum@(stack, sizes) line
  | "$ cd .." == line = changeDirOut accum
  | "$ cd" `isPrefixOf` line = (newDir line : stack, sizes)
  | "dir " `isPrefixOf` line = accum
  | "$ ls" == line = accum
  | otherwise = incrementDirSize (newFile line) accum
  where
    newDir str = Directory (last (words str)) 0
    newFile str = File (last (words str)) (read (head (words str)))

changeDirOut :: ([Directory], [Int]) -> ([Directory], [Int])
changeDirOut ((Directory dirName dirSize) : (Directory parentDirName parentDirSize) : restOfStack, sizes) = (Directory parentDirName (dirSize + parentDirSize) : restOfStack, dirSize : sizes)
changeDirOut ((Directory dirName dirSize) : restOfStack, sizes) = (restOfStack, dirSize : sizes)

incrementDirSize :: File -> ([Directory], [Int]) -> ([Directory], [Int])
incrementDirSize (File _ fileSize) ((Directory dirName dirSize) : restOfStack, sizes) = (Directory dirName (dirSize + fileSize) : restOfStack, sizes)

emptyStack :: ([Directory], [Int]) -> ([Directory], [Int])
emptyStack accum@([], sizes) = accum
emptyStack accum@(stack, sizes) = emptyStack $ changeDirOut accum
