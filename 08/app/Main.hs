module Main where

import Parse (parseFile, Network)
import System.IO ( hGetContents, openFile, IOMode(ReadMode) )
import Network(solvePart1, solvePart2)

part1 :: String -> String
part1 contents = case parseFile contents of 
    Just n -> show $ solvePart1 n
    Nothing -> "Invalid network!"


part2 :: String -> String
part2 contents = case parseFile contents of 
    Just n -> show $ solvePart2 n
    Nothing -> "Network invalid!"

solveForPart :: String -> (Network -> Integer) -> String
solveForPart contents solver = case parseFile contents of
    Just n -> show $ solver n
    Nothing -> "Could not parse network!"

main :: IO ()
main = do 
    fileHandle <- openFile "resources/input.txt" ReadMode
    contents <- hGetContents fileHandle
    let solver = solveForPart contents
    print $ solver solvePart1
    print $ solver solvePart2
