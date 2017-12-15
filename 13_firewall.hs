inputString :: String
inputString = "0: 5\n1: 2\n2: 3\n4: 4\n6: 6\n8: 4\n10: 8\n12: 6\n14: 6\n16: 8\n18: 6\n20: 9\n22: 8\n24: 10\n26: 8\n28: 8\n30: 12\n32: 8\n34: 12\n36: 10\n38: 12\n40: 12\n42: 12\n44: 12\n46: 12\n48: 14\n50: 12\n52: 14\n54: 12\n56: 14\n58: 12\n60: 14\n62: 14\n64: 14\n66: 14\n68: 14\n70: 14\n72: 14\n76: 14\n80: 18\n84: 14\n90: 18\n92: 17\n"

input :: [[String]]
input = map (map (filter (/= ':')) . words) $ lines inputString

-- |Direction
data Dir = Up | Down
         deriving (Eq, Show)

-- |Represents current situation in a layer
--  depth (number of layer)
--  range
--  current position of scanner
--  current direction of scanner
type Layer = (Int, Int, Int, Dir)

-- |Returns layers from input
getLayers :: [Layer]
getLayers = [ (read . head $ line,read . last $ line,0,Up) | line <- input]

-- |Returns the maximum number of layer
getMaxLayer :: Int
getMaxLayer = maximum [read . head $ line | line <- input]

-- |How does the scanner move
newPosAndDir :: Int -> Int -> Dir -> (Int, Dir)
newPosAndDir range pos dir = if dir == Up then
                                          if pos + 1 < range - 1 then (pos+1,Up)
                                          else (pos+1, Down)
                             else 
                                          if pos == 1 then (0,Up)
                                          else (pos - 1, Down)

-- |Given layer performs a step
layerStep :: Layer -> Layer
layerStep (n, range, pos, dir) = (\(npos, ndir) -> (n, range, npos, ndir)) $ newPosAndDir range pos dir

-- |In a list of layers, all of them perform a step
layersStep :: [Layer] -> [Layer]
layersStep = map layerStep

-- |Performs a step 
--  player does a step
--  layers do a step
--  returns: new player position
--           new layers
--           penalization
step :: Int -> [Layer] -> (Int, [Layer], Int)
step n lay = (n+1, layersStep lay, getPenalization (n+1) lay)

-- |Params: layer where player enters
--          state of layers as he enters it
getPenalization :: Int -> [Layer] -> Int
getPenalization n lays = getPenalization' $ findLayer n lays

getPenalization' :: Maybe Layer -> Int
getPenalization' Nothing = 0
getPenalization' (Just (depth,range,pos,_)) = if pos == 0 then depth * range
                                                          else 0

-- |Gets a number and returns the layer with that depth
findLayer :: Int -> [Layer] -> Maybe Layer
findLayer _ [] = Nothing
findLayer n ((a,b,c,d):xs) = if n == a then Just (a,b,c,d) else findLayer n xs

-- |Runs the game, returns penalization
run :: Int -> [Layer] -> Int -> Int -> Int
run pos layers pen finish = if pos > finish then pen
                            else (\(nPos, nLay, nPen) -> run nPos nLay (pen+nPen) finish) $ step pos layers

-- |Result to the first part - penalization for the whole run
result1 = run (-1) getLayers 0 getMaxLayer

-- |Returns the list of depths and ranges from the input
depthRange :: [(Int, Int)]
depthRange = [ (read . head $ line,read . last $ line) | line <- input]

-- |Player waits for n steps in the beginning
-- and then starts. This returns whether he gets caught
--
-- The player if caught if in any layer, this holds
--  (DELAY + DEPTH) MOD ((RANGE-1)*2) == 0
--
-- I needed to count it this way, because just trying to compute it
-- was way too inefficient
penWithDelay :: Int -> Bool
penWithDelay delay = any (\(depth,range) -> (delay + depth) `mod` ((range - 1)*2) == 0) depthRange

-- |Perfect delay: Player will not get caught
-- Finds smallest positive perfect delay - result to part 2
smallestPerfectDelay :: Int
smallestPerfectDelay = smallestPerfectDelay' 0

smallestPerfectDelay' :: Int -> Int
smallestPerfectDelay' n = if not $ penWithDelay n then n
                             else smallestPerfectDelay' $ n+1

result2 :: Int
result2 = smallestPerfectDelay
