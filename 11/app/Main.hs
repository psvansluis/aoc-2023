{-# LANGUAGE NumericUnderscores #-}

module Main (main) where

import Galaxy (fromString, pairwiseDistances)

solve :: String -> String
solve file = "Part 1: " ++ show p1 ++ "  Part 2:" ++ show p2
  where
    cosmos = fromString file
    distForFactor = pairwiseDistances cosmos
    [p1, p2] = map distForFactor [2, 1_000_000]

main :: IO ()
main = do
  example <- readFile "./resources/example.txt"
  print $ solve example
  input <- readFile "./resources/input.txt"
  print $ solve input