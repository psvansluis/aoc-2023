{-# LANGUAGE OverloadedRecordDot #-}
module Network (
    solvePart1,
    solvePart2
) where

import Parse (Network (..), network, Direction, directions, nodes, l)

import Data.Map (Map, (!), keys)

-- Part 1
solvePart1 :: Network -> Integer
solvePart1 nw = answer
    where 
        (_, _, answer) = until isAtZZZ doStep first
        isAtZZZ = \(_, pos, _) -> pos == "ZZZ"
        first = (nw, "AAA", 0)

doStep :: (Network, String, Integer) -> (Network, String, Integer)
doStep (currNetwork, currPos, currI) = (nextNetwork, nextPos, nextI)
    where 
        (dir: dirs) = currNetwork.directions
        nextNetwork = network dirs currNetwork.nodes
        nextPos = takeDirection currNetwork.nodes dir currPos
        nextI = currI + 1


takeDirection :: Map String (String, String) -> Direction -> String -> String
takeDirection nodes' dir key = if dir == l then lhs else rhs
    where 
        (lhs, rhs) = nodes' ! key

    
-- Part 2
solvePart2 :: Network -> Integer
solvePart2 nw = answer
    where
        answer = foldl1 Prelude.lcm listOfStepsNeeded
        listOfStepsNeeded = map stepsToSolve firsts
        firsts = map (\pos -> (nw, pos, 0)) $ nodesInA nw
        endsInZ (_,str,_) = last str == 'Z'
        stepsToSolve first = steps where (_, _, steps) = until endsInZ doStep first

nodesInA :: Network -> [String]
nodesInA nw = filter (\key -> last key == 'A') $ keys nw.nodes 


