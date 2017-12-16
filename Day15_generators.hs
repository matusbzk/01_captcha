module Day15_generators (result1, result2) where

import Data.Bits

data Generator = A | B
               deriving (Eq, Show)

start :: Generator -> Int
start A = 783
start B = 325

testStart :: Generator -> Int
testStart A = 65
testStart B = 8921

factor :: Generator -> Int
factor A = 16807
factor B = 48271

-- |Returns next value for a generator
nextValue :: Generator -> Int -> Int
nextValue gen prev = prev * factor gen `mod` 2147483647

-- |Infinite list of generated numbers
generated :: Generator -> [Int]
generated gen = tail $ iterate (nextValue gen) (start gen)

-- |Takes two numbers and returns whether their last 16 bits match
equalLast16Bits :: Int -> Int -> Bool
equalLast16Bits x y = x `mod` 2^16 == y `mod` 2^16

-- |Computes 40 milion pairs and for each of them
--  returns whether their last 16 bits match
boolList40Mil :: [Bool]
boolList40Mil = zipWith (==) (map (.&. 0xffff) . take 40000000 $ generated A) 
                             (map (.&. 0xffff) . take 40000000 $ generated B)

-- |Takes a list of bools and returns how many of them are true
numTrue :: [Bool] -> Int
numTrue [] = 0
numTrue (True:xs) = 1 + numTrue xs
numTrue (_:xs) = numTrue xs

result1 :: Int
result1 = numTrue boolList40Mil

criteria :: Generator -> Int
criteria A = 4
criteria B = 8

-- |Returns next value for a generator - part 2 version
nextValue2 :: Generator -> Int -> Int
nextValue2 gen prev = if nv `mod` criteria gen == 0 then nv
                                                    else nextValue2 gen nv
                where nv = nextValue gen prev

-- |Infinite list of generated numbers - part 2 version
generated2 :: Generator -> [Int]
generated2 gen = tail $ iterate (nextValue2 gen) (start gen)

-- |Computes 40 milion pairs and for each of them
--  returns whether their last 16 bits match  - part 2 version
boolList5Mil :: [Bool]
boolList5Mil = zipWith (==) (map (.&. 0xffff) . take 5000000 $ generated2 A) 
                            (map (.&. 0xffff) . take 5000000 $ generated2 B)

result2 :: Int
result2 = numTrue boolList5Mil