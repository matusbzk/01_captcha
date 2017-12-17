module Day17_spinlock (result1, result2) where

import Data.List
import AoC2017

-- |Puzzle input
steps :: Int
steps = 314

-- |Represents current state of the buffer
--  position, and actual buffer
type State = (Int,[Int])

-- |Computes a new position, from the current position
-- and the length of the buffer
newPosition :: Int -> Int -> Int
newPosition pos len = (pos + steps) `mod` len + 1

-- |Inserts a new number to a buffer
-- and also sets new position
insertNewNum :: State -> State
insertNewNum (pos,buf) = (newPos, newBuf)
                      where newPos = newPosition pos len
                            newBuf = take newPos buf ++ len : drop newPos buf
                            len = length buf

-- |Inicial state of the buffer
inicialState :: State
inicialState = (0,[0])

-- |State after 2017 insertions
finalState :: State
finalState = iterateN 2017 insertNewNum inicialState

-- |Returns a value after given number
valueAfter :: Int -> [Int] -> Int
valueAfter v xs = valueAfter' (head xs) v xs

-- |I need to remember the first value in the first argument
-- because the list is circular
valueAfter' :: Int -> Int -> [Int] -> Int
valueAfter' h v [x] = if v == x then h else error "There is no such value"
valueAfter' h v (x:xs) = if v == x then head xs else valueAfter' h v xs


-- |Result to part 1 - value after 2017 in buffer after 2017 insertions
result1 :: Int
result1 = valueAfter 2017 $ snd finalState

-- |Represents current state - part 2 version:
-- there is no need to remember position of zero, since it is always
-- in the beginning
--  value after zero
--  current position
--  current size
type State2 = (Int, Int, Int)

-- |Inicial state - part 2 version
inicialState2 :: State2
inicialState2 = (0,0,1)

-- |Computes next state - part 2 version
insertNewNum2 :: State2 -> State2
insertNewNum2 (val,pos,size) = (nval,npos,nsize)
                     where nsize = size + 1
                           npos = newPosition pos size
                           nval = if npos == 0 then size else val

-- |State after 50 mil iterations - part 2 version
finalState2 :: State2
finalState2 = iterateN 50000000 insertNewNum2 inicialState2

-- |Result to part 2 - value after 0 in buffer after 50 mil insertions
-- This still causes stack overflow. I was able to compute the solution
-- with some bang patterns - I copied that from u/nonphatic so I'm not
-- posting that here
result2 :: Int
--result2 = snd3 finalState2       --too slow, causes stack overflow
result2 = fst3 $ foldl' (\(val, pos, size) _ -> let npos = (pos + steps) `mod` size + 1 in
                    (if npos == 1 then size else val,
                    npos, 
                    size + 1) ) inicialState2 [1..50000000]
