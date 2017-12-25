module Day24_bridges (result1, result2) where

import AoC2017

-- |Represents a component
type Component = (Int, Int)

-- |Represents a bridge - all components need to be connected
type Bridge = [Component]

inputString :: String
inputString = "31/13\n34/4\n49/49\n23/37\n47/45\n32/4\n12/35\n37/30\n41/48\n0/47\n32/30\n12/5\n37/31\n7/41\n10/28\n35/4\n28/35\n20/29\n32/20\n31/43\n48/14\n10/11\n27/6\n9/24\n8/28\n45/48\n8/1\n16/19\n45/45\n0/4\n29/33\n2/5\n33/9\n11/7\n32/10\n44/1\n40/32\n2/45\n16/16\n1/18\n38/36\n34/24\n39/44\n32/37\n26/46\n25/33\n9/10\n0/29\n38/8\n33/33\n49/19\n18/20\n49/39\n18/39\n26/13\n19/32\n"

input :: [Component]
input = map ((\(x:[y]) -> (read x, read y)) . words . replace '/' ' ') . lines $ inputString

-- |Finds all components that contain given number
findComponents :: Int -> [Component] -> [Component]
findComponents _ [] = []
findComponents n ((x,y):xs) = if x == n || y == n then (x,y) : findComponents n xs
                                                  else findComponents n xs

-- |Finds all bridges starting at given port, that can use given components
findBridges :: Int -> [Component] -> [Bridge]
findBridges _ [] = []
findBridges n comps = map (:[]) components ++ 
                      concat (map (\(x,y) -> let other = if n == x then y else x in
                           map ((x,y):) (findBridges other (remove (x,y) comps))) components)
                   where components = findComponents n comps

-- |Removes all occurences of an element from the list
remove :: Eq a => a -> [a] -> [a]
remove _ [] = []
remove n (x:xs) = if n == x then remove n xs else x:remove n xs

-- |Returns strength of the bridge
getStrength :: Bridge -> Int
getStrength br = sum $ map (\(x,y) -> x+y) br

-- |Returns the strength of the strongest bridge from the list
getStrongest :: [Bridge] -> Int
getStrongest = maximum . map getStrength

-- |Returns the length of the longest bridge
getLongest :: [Bridge] -> Int
getLongest = maximum . map length

-- |Only keeps the longest bridges
keepLongest :: [Bridge] -> [Bridge]
keepLongest brs = filter (\br -> length br == l) brs
          where l = getLongest brs

-- |Gets the strength of the strongest bridge starting at 0
result1 :: Int
result1 = getStrongest $ findBridges 0 input

-- |Gets the strength of the longest bridge. If there are more,
-- chooses the strongest
result2 :: Int
result2 = getStrongest . keepLongest $ findBridges 0 input
