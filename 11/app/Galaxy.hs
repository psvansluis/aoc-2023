{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}

module Galaxy
  ( fromString,
    pairwiseDistances,
  )
where

import Data.List (elemIndices, findIndices, sort, transpose)

data Cosmos = Cosmos
  { galaxies :: [Galaxy],
    emptyRows :: [Int],
    emptyCols :: [Int]
  }
  deriving (Show)

data Galaxy = Galaxy {row :: Int, col :: Int} deriving (Show)

fromString :: String -> Cosmos
fromString str = Cosmos {galaxies, emptyRows, emptyCols}
  where
    emptyCols = expanders t
    emptyRows = expanders rows
    expanders = findIndices $ all (== '.')
    galaxies = linesToGalaxies rows
    rows = lines str
    t = transpose rows

linesToGalaxies :: [String] -> [Galaxy]
linesToGalaxies ls = galaxies
  where
    galaxies = coords >>= unpackCoords
    rowCoords = map (elemIndices '#') ls
    coords = zip [0 ..] rowCoords

unpackCoords :: (Int, [Int]) -> [Galaxy]
unpackCoords (row, col : cols) = Galaxy {row, col} : unpackCoords (row, cols)
unpackCoords (_, []) = []

pairs :: [Galaxy] -> [(Galaxy, Galaxy)]
pairs [] = []
pairs (g : gs) = map (g,) gs ++ pairs gs

dist :: Int -> Cosmos -> Galaxy -> Galaxy -> Int
dist factor Cosmos {emptyRows, emptyCols} g1 g2 = total
  where
    lowHigh dim = sort [dim g1, dim g2]
    [lowRow, highRow] = lowHigh row
    [lowCol, highCol] = lowHigh col
    emptyRowsBetween = length $ filter (\r -> r > lowRow && r < highRow) emptyRows
    emptyColsBetween = length $ filter (\c -> c > lowCol && c < highCol) emptyCols
    expansion = (emptyRowsBetween + emptyColsBetween) * (factor - 1)
    total = (highRow - lowRow) + (highCol - lowCol) + expansion

pairwiseDistances :: Cosmos -> Int -> Int
pairwiseDistances cosmos factor = sum distances
  where
    distances = map (uncurry (dist factor cosmos)) $ pairs $ galaxies cosmos