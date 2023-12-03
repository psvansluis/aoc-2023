module Main (main) where

import Lib (parseGames, part1Answer, part2Answer)
import System.IO

main :: IO ()
main = do
  fileHandle <- openFile "resources/input.txt" ReadMode
  contents <- hGetContents fileHandle
  print $ part1Answer (parseGames contents)
  print $ part2Answer (parseGames contents)
  hClose fileHandle