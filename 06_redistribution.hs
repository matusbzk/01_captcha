import Data.Maybe
import Data.List

inputString :: String
inputString = "10\t3\t15\t10\t5\t15\t5\t15\t9\t2\t5\t8\t5\t2\t3\t6\n"

-- |Represents a current state of distrubition of blocks into banks
type State = [Int]

-- |Distribution state in the beginning
banksStart :: State
banksStart = map read . words $ inputString

-- |Takes an element x and a list xs. Returns index of first appearance of x in xs
-- |If x is not in xs, it's an error
firstEqualIndex :: Eq a => a -> [a] -> Int
firstEqualIndex x xs = fromJust $ findIndex (== x) xs

-- |Performs a one iteration of reallocation
doAnIteration :: State -> State
doAnIteration banks = insertIntoNext ((maxIndex + 1) `mod` length banks) highest afterRemoving
                        where highest = maximum banks
                              maxIndex = firstEqualIndex highest banks
                              afterRemoving = removeFromSelected maxIndex banks

-- |Removes all blocks from a selected bank
-- | arguments:
-- |  1 : index of the selected bank
-- |  2 : banks
-- |return: how the banks look now
removeFromSelected :: Int -> State -> State
removeFromSelected 0 (x:xs) = 0 : xs
removeFromSelected n (x:xs) = x : removeFromSelected (n-1) xs

-- |Redistributes n blocks
-- | arguments:
-- |  1 : into which bank should I insert next?
-- |  2 : how many blocks left to insert
-- |  3 : banks
-- | return: how the banks look now
insertIntoNext :: Int -> Int -> State -> State
insertIntoNext _ 0 banks = banks
insertIntoNext i n banks = insertIntoNext ((i+1) `mod` length banks) (n-1)
                            $ take i banks ++ ((banks !! i) + 1) : drop (i+1) banks

-- |Returns a number of iterations before a configuration is produced that has been seen before
-- |Result to part one
numberOfIterations :: Int
numberOfIterations = run 0 banksStart []

-- |Computes iterations until a configuration is produced that has been seen before
-- | and counts the iterations
run :: Int -> State -> [State] -> Int
run n banks history = if elem newBanks newHistory then n+1
                                                  else run (n+1) newBanks newHistory
                           where newBanks = doAnIteration banks
                                 newHistory = banks : history

-- |Returns a length of a given cycle
-- |Result to part 2
lengthOfCycle :: Int
lengthOfCycle = run2 0 banksStart []

-- |Computes iterations until a configuration is produced that has been seen before
-- | and then counts the length of a cycle
run2 :: Int -> State -> [State] -> Int
run2 n banks history = if elem newBanks newHistory 
                       then firstEqualIndex newBanks newHistory + 1
                       else run2 (n+1) newBanks newHistory
                           where newBanks = doAnIteration banks
                                 newHistory = banks : history
