module Pattern
  ( parseToPattern,
    verticalLinesOfReflection,
    horizontalLinesOfReflection,
    solvePart1,
  )
where

import Data.List (intersect, transpose)
import Data.List.Split (endBy)

type Pattern = [String]

parseToPattern :: String -> [Pattern]
parseToPattern str = parseLines $ lines str

parseLines :: [String] -> [Pattern]
parseLines [] = []
parseLines ("" : xs) = parseLines xs
parseLines ls = left : parseLines right
  where
    (left, right) = break null ls

splits :: String -> [(String, String)]
splits str = map split [1 .. length str - 1]
  where
    split i = splitAt i str

isMirrored :: (String, String) -> Bool
isMirrored (lhs, rhs) = all (uncurry (==)) zipped
  where
    zipped = zip (reverse lhs) rhs

splitIndicesAtLine :: String -> [Int]
splitIndicesAtLine str = map (\(f, _) -> length f) $ filter isMirrored $ splits str

verticalLinesOfReflection :: Pattern -> [Int]
verticalLinesOfReflection [] = []
verticalLinesOfReflection [l] = splitIndicesAtLine l
verticalLinesOfReflection (l : ls) = splitIndicesAtLine l `intersect` verticalLinesOfReflection ls

horizontalLinesOfReflection :: Pattern -> [Int]
horizontalLinesOfReflection pattern = verticalLinesOfReflection $ transpose pattern

sumHeads :: [[Int]] -> Int
sumHeads [] = 0
sumHeads ([] : xs) = sumHeads xs
sumHeads ((xHead : _) : xs) = xHead + sumHeads xs

solvePart1 :: [Pattern] -> Int
solvePart1 patterns = colSum + (rowSum * 100)
  where
    colSum = sumHeads $ map verticalLinesOfReflection patterns
    rowSum = sumHeads $ map horizontalLinesOfReflection patterns