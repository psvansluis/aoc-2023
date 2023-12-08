module Network (
    solvePart1
) where

import Parse (Network (..), network, Direction, directions, nodes, left, right)

import Data.Map (Map, (!))

solvePart1 :: Network -> Integer
solvePart1 nw = answer
    where 
        (_, _, answer) = until isAtZZZ doStep first
        isAtZZZ = \(_, pos, _) -> pos == "ZZZ"
        first = (nw, "AAA", 0)

doStep :: (Network, String, Integer) -> (Network, String, Integer)
doStep (currNetwork, currPos, currI) = 
    (nextNetwork, nextPos, nextI)
    where 
        nextNetwork = network (tail $ directions currNetwork) (nodes currNetwork)
        currDirection = head $ directions currNetwork
        nextPos = takeDirection (nodes currNetwork) currDirection currPos
        nextI = currI + 1


takeDirection :: Map String (String, String) -> Direction -> String -> String
takeDirection nodes' dir key = if dir == left then lhs else rhs
    where 
        (lhs, rhs) = nodes' ! key

    
