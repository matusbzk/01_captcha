module Day25_turing (result1) where

import Data.Foldable (foldl')
import qualified Data.IntSet as IntSet

-- |Current position of cursor
type Position = Int

-- |Current state of the tape
-- list of positions where there is 1
type Tape = IntSet.IntSet

-- |State of the Turing machine
data State = A | B | C | D | E | F
           deriving (Show, Eq)

-- |Current status of the Turing machine
--  cursor position
--  tape
--  current state
type Machine = (Position, Tape, State)

-- |Number of steps until checksum - from the input
steps :: Int
steps = 12919244

-- |Takes a position and the tape and returns given value
getValue :: Position -> Tape -> Int
getValue n tape = if IntSet.member n tape then 1 else 0

-- |Sets a value at a given position
setValue :: Position -> Tape -> Int -> Tape
setValue n tape v = if v == 0 then IntSet.delete n tape else IntSet.insert n tape

-- |Performs a step
-- taken from input
step :: Machine -> Machine
step (pos, tape, A) = if getValue pos tape == 1 then (pos-1,setValue pos tape 0,C)
                                                else (pos+1,setValue pos tape 1,B)
step (pos, tape, B) = if getValue pos tape == 1 then (pos+1,tape               ,D)
                                                else (pos-1,setValue pos tape 1,A)
step (pos, tape, C) = if getValue pos tape == 1 then (pos-1,setValue pos tape 0,E)
                                                else (pos+1,setValue pos tape 1,A)
step (pos, tape, D) = if getValue pos tape == 1 then (pos+1,setValue pos tape 0,B)
                                                else (pos+1,setValue pos tape 1,A)
step (pos, tape, E) = if getValue pos tape == 1 then (pos-1,tape               ,C)
                                                else (pos-1,setValue pos tape 1,F)
step (pos, tape, F) = if getValue pos tape == 1 then (pos+1,tape               ,A)
                                                else (pos+1,setValue pos tape 1,D)

-- |Starting state of the machine
startState :: Machine
startState = (0,IntSet.empty, A)

-- |Number of ones on the tape
checksum :: Machine -> Int
checksum (_, tape, _) = IntSet.size tape

-- |Runs 'steps' iterations of step
run = foldl' (\st _ -> step st) startState [1..steps]

-- |Number of ones aftes 'steps' iterations
result1 = checksum run
