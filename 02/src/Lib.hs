{-# LANGUAGE NamedFieldPuns #-}

module Lib
  ( part1Answer,
    part2Answer,
    parseGames,
  )
where

import Control.Monad.Zip (mzip)
import Data.List (find, maximumBy, stripPrefix)
import Data.Maybe (mapMaybe)
import Text.Read (readMaybe)

data Game = Game {index :: Int, samples :: [Sample]} deriving (Show)

data Sample = Sample {red :: Int, green :: Int, blue :: Int} deriving (Show)

data Color = Red | Green | Blue deriving (Eq, Show)

-- filtering and summing
sampleSatisfiesPart1 :: Sample -> Bool
sampleSatisfiesPart1 Sample {red, green, blue} = red <= 12 && green <= 13 && blue <= 14

gameSatisfiesPart1 :: Game -> Bool
gameSatisfiesPart1 Game {samples} = all sampleSatisfiesPart1 samples

part1Answer :: [Game] -> Int
part1Answer games = sum indices
  where
    filtered = filter gameSatisfiesPart1 games
    indices = map index filtered

smallestPossibleBag :: [Sample] -> Sample
smallestPossibleBag samples = Sample {red = maxCol red, green = maxCol green, blue = maxCol blue}
  where
    maxCol :: (Sample -> Int) -> Int
    maxCol color = maximum (map color samples)

power :: Game -> Int
power game = red bag * green bag * blue bag
  where
    bag = smallestPossibleBag samps
    samps = samples game

part2Answer :: [Game] -> Int
part2Answer games = sum $ map power games

-- parsing
parseGames :: String -> [Game]
parseGames str = mapMaybe parseGame (lines str)

parseGame :: String -> Maybe Game
parseGame str = parse' $ split ':' str

parse' :: [String] -> Maybe Game
parse' [lhs, rhs] = case parseIndex lhs of
  Just i -> Just Game {index = i, samples = parseSamples rhs}
  Nothing -> Nothing
parse' _ = Nothing

parseIndex :: String -> Maybe Int
parseIndex str = stripPrefix "Game " str >>= readMaybe

parseSamples :: String -> [Sample]
parseSamples str = map parseSample $ split ';' str

parseSample :: String -> Sample
parseSample str =
  Sample
    { red = findCol Red,
      green = findCol Green,
      blue = findCol Blue
    }
  where
    observations = mapMaybe parseObservation (split ',' str)
    findCol col = maybe 0 fst (find (\obs -> snd obs == col) observations)

parseObservation :: String -> Maybe (Int, Color)
parseObservation str = parseObs $ words str
  where
    parseObs [amount, color] = mzip (readMaybe amount) (readColor color)
    parseObs _ = Nothing
    readColor "red" = Just Red
    readColor "blue" = Just Blue
    readColor "green" = Just Green
    readColor _ = Nothing

split :: Char -> String -> [String]
split sep "" = []
split sep str = first : split sep (drop 1 rest)
  where
    (first, rest) = span (/= sep) str