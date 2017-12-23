module Day22_sporifica (result1, result2) where

import Prelude hiding (Left, Right)
import AoC2017
import Data.Foldable (foldl')

-- |Represents a current position
type Position = (Int, Int)

-- |List of infected cells
type Infected = [Position]

-- |Represents a direction
data Direction = Up | Down | Left | Right
               deriving (Show, Eq)

-- |Current state
--  current position
--  current direction
--  list of infected nodes
--  number of nodes I have infected
type State = (Position, Direction, Infected, Int)

inputString :: String
inputString = ".##..#.#.##...#....#..###\n####.#...###.####..#.....\n#.#.#####....######.###.#\n#.#..###.#.#####....#..#.\n####.#.#...#.##.##..#.###\n#.####..#####.#.#....#.##\n.#.####.#....###..##....#\n..##.#..##.#.#.###.##.#..\n##....#....######.###.###\n.#.##.###.###.###.#..#.#.\n#.##.#.#..#.#.....###....\n####.....#..###..##..##..\n##....#.#...####...#.#.#.\n...#.##..###..##..#......\n#....#..##.##.#..#.###..#\n...#...##.##.##...#.#.#..\n.##....#.####.#..##.#...#\n#.######......#.#...#.##.\n#.##....###...###.###....\n#..#.#.#.#.#..#.#.....#..\n...##..##.###....#.###...\n.######.#...###.###.#.#.#\n####..###.####...#..#####\n.##.#.##...##..##...#.#.#\n###...##..#..##.##..#..#.\n"

-- |Input is 25x25 grid
input :: Infected
input = getInfected . concat . insertPositions (-12) . lines $ inputString

-- |Insert position into input grid
insertPositions :: Int -> [[Char]] -> [[(Position, Char)]]
insertPositions _ [] = []
insertPositions i (xs:xss) = insertPositions' i (-12) xs : insertPositions (i+1) xss

insertPositions' :: Int -> Int -> [Char] -> [(Position, Char)]
insertPositions' _ _ [] = []
insertPositions' i j (x:xs) = ((j,i),x) : insertPositions' i (j+1) xs

-- |Returns infected positions
getInfected :: [(Position,Char)] -> Infected
getInfected [] = []
getInfected (((x,y),'#'):xs) = (x,y) : getInfected xs
getInfected (_:xs) = getInfected xs

-- |State in the beginning
startState :: State
startState = ((0,0),Up,input,0)

-- |Checks whether given position contains infected node
-- also used for flagged and weakened nodes
isInfected :: Position -> Infected -> Bool
isInfected = elem

-- |Infects node at given position
-- also used for flagged and weakened nodes
infect :: Position -> Infected -> Infected
infect (x,y) inf = if isInfected (x,y) inf then inf else (x,y):inf

-- |Cleans node at given position
-- also used for flagged and weakened nodes
clean :: Position -> Infected -> Infected
clean _ [] = []
clean p (x:xs) = if p == x then xs else x : clean p xs

-- |If given node is infected then clean it,
-- otherwise infect it
infectOrClean :: Position -> Infected -> Infected
infectOrClean pos inf = if isInfected pos inf then clean pos inf else infect pos inf

-- |Change direction to the right
turnRight :: Direction -> Direction
turnRight Up = Right
turnRight Right = Down
turnRight Down = Left
turnRight Left = Up

-- |Change direction to the left
turnLeft :: Direction -> Direction
turnLeft = turnRight . turnRight . turnRight

-- |From position and direction, takes one step forward
move :: Position -> Direction -> Position
move (x,y) Up = (x,y-1)
move (x,y) Down = (x,y+1)
move (x,y) Right = (x+1,y)
move (x,y) Left = (x-1,y)

-- |Performs one burst
burst :: State -> State
burst (pos,dir,inf,n) = (npos,ndir,ninf,nn)
                where ndir = if isInfected pos inf then turnRight dir else turnLeft dir
                      npos = move pos ndir
                      ninf = infectOrClean pos inf
                      nn = if not $ isInfected pos inf then n+1 else n

-- |How many cells were infected
result1 :: Int
result1 = (\(_,_,_,x) -> x) $ iterateN 10000 burst startState

-- |List of weakened cells
type Weakened = [Position]

-- |List of flagged cells
type Flagged = [Position]

-- |Contains a lists of all nodes affected by virus
type Virused = (Infected, Weakened, Flagged)

-- |State for part 2
type State2 = (Position, Direction, Virused, Int)

-- |Direction is reversed
reverseDir :: Direction -> Direction
reverseDir = turnRight . turnRight

-- |Start state - part 2 version
startState2 :: State2
startState2 = ((0,0),Up,(input,[],[]),0)

-- |Weakens/cleans/infects/flags a node
modifyNodes :: Position -> Virused -> Virused
modifyNodes pos (inf,wea,fla) = if isInfected pos inf then (clean pos inf, wea, infect pos fla)
                           else if isInfected pos wea then (infect pos inf, clean pos wea, fla)
                           else if isInfected pos fla then (inf, wea, clean pos fla)
                           else (inf, infect pos wea, fla)

-- |Performs one burst - part 2 version
burst2 :: State2 -> State2
burst2 (pos,dir,(inf,wea,fla),n) = (npos,ndir,(ninf,nwea,nfla),nn)
                where ndir = if isInfected pos inf then turnRight dir 
                             else if isInfected pos wea then dir 
                             else if isInfected pos fla then reverseDir dir
                             else turnLeft dir
                      npos = move pos ndir
                      (ninf,nwea,nfla) = modifyNodes pos (inf,wea,fla)
                      nn = if isInfected pos wea then n+1 else n

-- |Similar thing as burst2 but should be more effective
-- Not enough to solve it in reasonable time though.
burst2Fold = foldl' (\(pos,dir,(inf,wea,fla),n) _ -> 
                  let ndir = if isInfected pos inf then turnRight dir 
                             else if isInfected pos wea then dir 
                             else if isInfected pos fla then reverseDir dir
                             else turnLeft dir
                      npos = move pos ndir
                      (ninf,nwea,nfla) = modifyNodes pos (inf,wea,fla)
                      nn = if isInfected pos wea then n+1 else n
                  in (npos,ndir,(ninf,nwea,nfla),nn)) startState2 [1..10000000]

-- |How many cells were infected - part 2 version
-- Not effective enough though - it takes 15 second on 100 times less iterations
result2 :: Int
--result2 = (\(_,_,_,x) -> x) $ iterateN 100000 burst2 startState2 --ineffective
result2 = (\(_,_,_,x) -> x) $ burst2Fold  --a bit more effective
