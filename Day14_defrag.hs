module Day14_defrag (result1, result2) where

import Day10_hash (hash)
import Data.List

inputString :: String
inputString = "jxqlasbh"

-- |Inputs for hash function
inputs :: [String]
inputs = [inputString ++ "-" ++ show i | i <- [0..127]]

-- |List of hashes
hashes :: [String]
hashes = map hash inputs

hexToBinary :: Char -> String
hexToBinary '0' = "0000"
hexToBinary '1' = "0001"
hexToBinary '2' = "0010"
hexToBinary '3' = "0011"
hexToBinary '4' = "0100"
hexToBinary '5' = "0101"
hexToBinary '6' = "0110"
hexToBinary '7' = "0111"
hexToBinary '8' = "1000"
hexToBinary '9' = "1001"
hexToBinary 'a' = "1010"
hexToBinary 'b' = "1011"
hexToBinary 'c' = "1100"
hexToBinary 'd' = "1101"
hexToBinary 'e' = "1110"
hexToBinary 'f' = "1111"

-- |Hashes, converted to binary
binHashes :: [String]
binHashes = map (concat . map hexToBinary) hashes

-- |Returns number of ones in a string
ones :: String -> Int
ones "" = 0
ones ('1':xs) = 1 + ones xs
ones (_:xs) = ones xs

-- |Number of ones in the binary hashes - result to part 1
numberOfOnes :: Int
numberOfOnes = sum $ map ones binHashes

result1 :: Int
result1 = numberOfOnes

-- |Groups only by lines
byLines :: [[Char]] -> [[Int]]
byLines xs = tail . map fst $ scanl (\(l,x) line -> onLine line (x+1) ) ([],0) xs

-- |Forms a group on a single line
--  params: line
--          which number to begin with
onLine :: String -> Int -> ([Int],Int)
onLine line start = (\(list, x) -> (reverse list, x)) $ onLine' start [] False line

onLine' :: Int -> [Int] -> Bool -> String -> ([Int],Int)
onLine' n acc _ "" = (acc,n)
onLine' n acc False ('0':xs) = onLine' n (0:acc) False xs
onLine' n acc True ('0':xs) = onLine' (n+1) (0:acc) False xs
onLine' n acc _ ('1':xs) = onLine' n (n:acc) True xs

-- |Groups by lines and columns - not combined
byLinesAndCols :: [[(Int,Int)]]
byLinesAndCols = [ [ (byLins!!x!!y,byCols!!y!!x) | x <- [0..127]] | y <- [0..127]]
                   where byLins = byLines binHashes
                         byCols = byLines . transpose $ binHashes

-- |Every used square, with groupings from byLinesAndCols
toMerge :: [([Int],[Int])]
toMerge = map (\(a,b) -> ([a],[b])) . concat $ map (filter (/= (0,0))) byLinesAndCols

-- |Merges all squares into regions
merge :: [([Int],[Int])] -> [([Int],[Int])]
merge [] = []
merge ((a,b):xs) = fst iter : merge (snd iter)
                       where iter = merge' (a,b) [] xs

merge' :: ([Int], [Int]) -> [([Int],[Int])] -> [([Int],[Int])] -> (([Int],[Int]), [([Int],[Int])])
merge' (a,b) acc [] = ((a,b),acc)
merge' (a,b) acc ((c,d):xs)
   | commonElem a c || commonElem b d = merge' (union a c,union b d) [] (acc ++ xs)
   | otherwise                        = merge' (a,b) ((c,d):acc) xs

-- |Number of regions - result to part 2
result2 :: Int
result2 = length $ merge toMerge

-- |Returns whether two lists contain common element
commonElem :: Eq a => [a] -> [a] -> Bool
commonElem [] ys = False
commonElem (x:xs) ys = elem x ys || commonElem xs ys
