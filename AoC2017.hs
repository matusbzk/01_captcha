-- This module contains some basic functions
-- I am using in my solutions

module AoC2017 
 ( fst3,
   snd3,
   thd3,
   hasDuplicates,
   iterateN,
   isNum,
   isPrime) where

import Data.Char
import Primes  --Data.Numbers.Primes

fst3 :: (a,b,c) -> a
fst3 (x,_,_) = x

snd3 :: (a,b,c) -> b
snd3 (_,x,_) = x

thd3 :: (a,b,c) -> c
thd3 (_,_,x) = x

-- |Returns true iff the list contains at least one duplicate element
hasDuplicates :: Eq a => [a] -> Bool
hasDuplicates [] = False
hasDuplicates (x:xs) = if elem x xs then True else hasDuplicates xs

-- |Iterates a function n  times
iterateN :: Int -> (a -> a) -> a -> a
iterateN n f = foldr (.) id (replicate n f)

-- |Returns whether char is digit or '-'
isNum :: Char -> Bool
isNum '-' = True
isNum x = isDigit x
