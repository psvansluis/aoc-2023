module Main where

import Pattern (parseToPattern, solvePart1)

main :: IO ()
main = do
  file <- readFile "./resources/input.txt"
  let patterns = parseToPattern file
  print patterns
  print $ solvePart1 patterns
