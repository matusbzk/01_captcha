{-# LANGUAGE BangPatterns #-}
module Day25_turing (result1, result2, main) where

import Data.Foldable (foldl')

-- |Current position of cursor
type Position = Int

-- |Current state of the tape
-- first list is from 0 to infinity
-- second is from -1 to -infinity
data Tape = Tap [Int] [Int]
          deriving Eq

instance Show Tape where
 show (Tap x y) = "Some tape"

-- |State of the Turing machine
data State = A | B | C | D | E | F
           deriving (Show, Eq)

-- |Current status of the Turing machine
--  cursor position
--  tape
--  current state
--  maximal absolute value of position where it got - I need it to compute checksum
type Machine = (Position, Tape, State, Int)

-- |Number of steps until checksum - from the input
steps :: Int
steps = 1291924--4

-- |Takes a position and the tape and returns given value
getValue :: Position -> Tape -> Int
getValue n (Tap pos neg) = if n >= 0 then pos!!n
                                     else neg!!(-n-1)

-- |Sets a value at a given position
setValue :: Position -> Tape -> Int -> Tape
setValue n (Tap pos neg) v = if n >= 0 then Tap (take n pos ++ v : drop (n+1) pos) neg
                                       else Tap pos (take (-n-1) neg ++ v : drop (-n) neg)

-- |Performs a step
-- taken from input
step :: Machine -> Machine
step !(pos, tape, A, mx) = if getValue pos tape == 0 then (pos+1,setValue pos tape 1,B, nmx)
                                                    else (pos-1,setValue pos tape 0,C, nmx)
                 where nmx = max (abs pos + 1) mx
step !(pos, tape, B, mx) = if getValue pos tape == 0 then (pos-1,setValue pos tape 1,A, nmx)
                                                    else (pos+1,tape,               D, nmx)
                 where nmx = max (abs pos + 1) mx
step !(pos, tape, C, mx) = if getValue pos tape == 0 then (pos+1,setValue pos tape 1,A, nmx)
                                                    else (pos-1,setValue pos tape 0,E, nmx)
                 where nmx = max (abs pos + 1) mx
step !(pos, tape, D, mx) = if getValue pos tape == 0 then (pos+1,setValue pos tape 1,A, nmx)
                                                    else (pos+1,setValue pos tape 0,B, nmx)
                 where nmx = max (abs pos + 1) mx
step !(pos, tape, E, mx) = if getValue pos tape == 0 then (pos-1,setValue pos tape 1,F, nmx)
                                                    else (pos-1,tape               ,C, nmx)
                 where nmx = max (abs pos + 1) mx
step !(pos, tape, F, mx) = if getValue pos tape == 0 then (pos+1,setValue pos tape 1,D, nmx)
                                                    else (pos+1,tape               ,A, nmx)
                 where nmx = max (abs pos + 1) mx

-- |Starting state of the machine
startState :: Machine
startState = (0,Tap (repeat 0) (repeat 0), A, 0)

-- |Number of ones on the tape
checksum :: Machine -> Int
checksum (_, Tap pos neg, _, mx) = sum (take mx pos) + sum (take mx neg)

run = foldl' (\st _ -> step st) startState [1..steps]

result1 = checksum run

main = print result1

result2 = undefined
