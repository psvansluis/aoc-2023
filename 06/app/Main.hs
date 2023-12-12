module Main where

import Race (parse, countWaysToBeat)

prod :: [Int] -> Int
prod [] = 1
prod (x:xs) = x * prod xs

main :: IO ()
main = do
    file <- readFile "./resources/input.txt"
    let races = parse file
    print $ races
    print $ prod $ map countWaysToBeat races
