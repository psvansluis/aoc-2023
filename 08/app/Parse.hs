{-# LANGUAGE NamedFieldPuns #-}

module Parse (
    parseFile,
    Direction, 
    Network, 
    network,
    directions,
    nodes,
    left,
    right
) where 

import Data.Maybe (mapMaybe)
import Data.Char (isUpper)
import Data.Map (Map, fromList)

data Direction = L | R deriving (Eq, Show)
data Network = Network {directions:: [Direction], nodes:: Map String (String, String)} deriving (Eq, Show)

left :: Direction
left = L

right :: Direction
right = R

network :: [Direction] -> Map String (String, String) -> Network
network directions nodes = Network{directions, nodes}

parseDirections :: String -> [Direction]
parseDirections str = cycle $ mapMaybe parseDirection str

parseDirection :: Char -> Maybe Direction
parseDirection 'L' = Just L
parseDirection 'R' = Just R
parseDirection _ = Nothing

parseNode :: String -> (String, (String, String))
parseNode str = let
    spl ch = span (/= ch)
    clean = filter isUpper 
    (origin, targets) = spl '=' str
    (left, right) = spl ',' targets
        in
    (clean origin, (clean left, clean right))

parseNodes :: [String] -> Map String (String, String)
parseNodes ls = fromList $ map parseNode ls

parseLines :: [String] -> Maybe Network
parseLines (dirStr: _: nodesStr) = Just $ network (parseDirections dirStr) (parseNodes nodesStr)
parseLines _ = Nothing

parseFile :: String -> Maybe Network
parseFile str = do parseLines $ lines str