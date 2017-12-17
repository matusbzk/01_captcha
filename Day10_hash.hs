module Day10_hash (hash, result1, result2) where

import Data.Char
import Data.Bits
import AoC2017

startList :: [Int]
startList = [0..255]

lengths :: [Int]
lengths = [97,167,54,178,2,11,209,174,119,248,254,0,255,1,64,190]

lengthsString :: String
lengthsString = "97,167,54,178,2,11,209,174,119,248,254,0,255,1,64,190"

-- |Saves the current state
--   position
--   list
--   current skip size
type State = (Int, [Int], Int)

-- |State in the beginning
startState :: State
startState = (0, startList, 0)

-- |Runs the algorithm - takes start state and sequence of lenghts,
--  returns final state
run :: State -> [Int] -> State
run state [] = state
run before (len:xs) = run (getNewState before len) xs

-- |From current state and given length, returns new state
getNewState :: State -> Int -> State
getNewState (pos, list, skip) len = ((pos + len + skip) `mod` length list, 
                  drop end reversed ++ take (pos-beg) (drop beg list) ++ take end reversed ++ drop (len+pos) list,
                  skip + 1)
       where reversed = reverse $ take end (drop pos list) ++ take beg list
             beg = max 0 $ len + pos - length list  --number of elements reversed in the beginning of the list
             end = min len $ length list - pos 

-- |Result to first part - product of first two numbers in result
result1 :: Int
result1 = (\(x:y:_) -> x*y) (snd3 $ run startState lengths)

-- |Takes a string and replaces each char with its ASCII value
stringToASCII :: String -> [Int]
stringToASCII = map ord

-- |Runs the algorithm 64 times - basically just repeats the lenghts
-- sequence 64 times and runs it
run64 :: State -> [Int] -> State
run64 state lens = run state $ take (length lens * 64) (cycle lens)

-- |Replace each 16 elements with their xor
doXor :: [Int] -> [Int]
doXor [] = []
doXor xs = foldr1 xor (take 16 xs) : doXor (drop 16 xs)

-- |Takes an int and converts it into hexadecimal
intToHex :: Int -> String
intToHex n = intToDigit (n `div` 16) : intToDigit (n `mod` 16) : []

-- |Prepares string to hashing
prepareString :: String -> [Int]
prepareString s = stringToASCII s ++ [17, 31, 73, 47, 23]

-- |Computes a hash from string
hash :: String -> String
hash s = concat . map intToHex . doXor . snd3 $ run64 startState (prepareString s)

-- |Result to second part - hash
result2 :: String
result2 = hash lengthsString
