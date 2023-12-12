module Race (
    parseRaces, 
    parseRace,
    countWaysToBeat
) where

import Data.Maybe (mapMaybe, fromJust)
import Text.Read (readMaybe)
import Data.List (find)
import Data.Char (isDigit)

data Race = Race {time :: Int, distance :: Int} deriving (Show)

parseRaces :: String -> [Race]
parseRaces str = map toRace pairs
    where
        [times, distances] = map readLine $ lines str
        readLine line = mapMaybe readMaybe $ words line
        pairs = zip times distances
        toRace (time, distance) = Race {time, distance}

parseRace :: String -> Race
parseRace str = Race {time, distance}
    where
        (time : distance : _) = map bigNum $ lines str
        bigNum line = read $ filter isDigit line

getDistance :: Int -> Int -> Int
getDistance time speed = (time - speed) * speed

firstSpeedToBeat :: [Int] -> Race ->  Int
firstSpeedToBeat iter Race {time, distance}  = fromJust $ find beats iter
    where beats speed = getDistance time speed > distance

countWaysToBeat :: Race -> Int
countWaysToBeat race = max 0 (maxSpeed - minSpeed + 1)
    where
        minSpeed = firstSpeedToBeat [1 .. time race] race
        maxSpeed = firstSpeedToBeat [time race, time race - 1 .. 1] race
