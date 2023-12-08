module Main where

import Parse (parseFile)
import System.IO ( hGetContents, openFile, IOMode(ReadMode) )
import Network(solvePart1)

part1 :: String -> String
part1 contents = case parseFile contents of 
    Just n -> show $ solvePart1 n
    Nothing -> "No solution!"


main :: IO ()
main = do 
    fileHandle <- openFile "resources/input.txt" ReadMode
    contents <- hGetContents fileHandle
    print $ part1 contents
