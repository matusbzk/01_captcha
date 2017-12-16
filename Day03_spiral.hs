module Day03_spiral (result1, result2) where

input :: Int
input = 289326

-- |How many steps in given direction is needed
-- >>> [1,1,2,2,3,3,4,4,5,5,...]
moves :: [Int]
moves = concat [[x,x] | x <- [1..]]

-- |Directions, in order
directions :: [String]
directions = cycle ["Right","Up","Left","Down"]

-- |Directions, with order and also right number of moves
whereToGo :: [String]
whereToGo = "No move" : concat [ replicate n x | (n,x) <- zip moves directions]

-- |Given position and direction, where will I be in next step
move :: (Int, Int) -> String -> (Int,Int)
move (x,y) "No move" = (x,y)
move (x,y) "Right" = (x+1,y)
move (x,y) "Up" = (x,y+1)
move (x,y) "Left" = (x-1,y)
move (x,y) "Down" = (x,y-1)

-- |After n steps, what will my position be (+ history stored in list)
whereAmI :: Int -> [(Int, Int)]
whereAmI n = foldl (\(loc:xs) dir -> move loc dir :loc:xs) [(0,0)] $ take n whereToGo

-- |Result of the first part - shortest path to given input
result1 = (\(x,y) -> abs x + abs y) $ head (whereAmI input)

-- |After n steps, what will my position and value in that position be (+history)
values :: Int -> [((Int, Int), Int)]
values 1 = [((0,0),1)]
values n = ( (x,y), sum [ v | ((a,b),v) <- recur, abs (x-a) <= 1, abs (y-b) <= 1] ) : recur
                 where (x,y) = head (whereAmI n)
                       recur = values (n-1)

-- |Result of the second part - first number bigger then input
result2 :: Int
result2 = checkN 1

-- |Finds the first value bigger then input
checkN :: Int -> Int
checkN n = if nth >= input then nth else checkN $ n+1
                where nth = snd $ head (values n)
