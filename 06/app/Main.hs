module Main where

import Race (parseRaces, parseRace, countWaysToBeat)

prod :: [Int] -> Int
prod [] = 1
prod (x:xs) = x * prod xs

main :: IO ()
main = do
    file <- readFile "./resources/input.txt"
    let races = parseRaces file
        race = parseRace file
        answerPart1 = prod $ map countWaysToBeat races
        answerPart2 = countWaysToBeat race
    print $ races
    print $ answerPart1
    print $ race
    print $ answerPart2
