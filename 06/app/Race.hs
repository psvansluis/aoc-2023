module Race (
    parse, 
    Race,
    countWaysToBeat
) where

import Data.Maybe (mapMaybe, fromJust)
import Text.Read (readMaybe)
import Data.List (find)

data Race = Race {time :: Int, distance :: Int} deriving (Show)

parse :: String -> [Race]
parse str = map toRace pairs
    where
        [timeLine, distanceLine] = lines str
        times = mapMaybe readMaybe $ words timeLine
        distances = mapMaybe readMaybe (words distanceLine)
        pairs = zip times distances
        toRace (time, distance) = Race {time, distance}

getDistance :: Int -> Int -> Int
getDistance time speed = (time - speed) * speed

minSpeedToBeat :: Race -> Int
minSpeedToBeat = firstSpeedToBeat [1..]

maxSpeedToBeat :: Race -> Int
maxSpeedToBeat race = firstSpeedToBeat (map (\x -> (time race) - x) [1..]) race

firstSpeedToBeat :: [Int] -> Race ->  Int
firstSpeedToBeat iter Race {time, distance}  = fromJust $ find beats iter
    where beats speed = getDistance time speed > distance

countWaysToBeat :: Race -> Int
countWaysToBeat race
    | max < min = 0
    | otherwise = max - min + 1
    where
        min = minSpeedToBeat race
        max = maxSpeedToBeat race
