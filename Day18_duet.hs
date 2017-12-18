module Day18_duet (result1, result2) where

import AoC2017
import Data.Maybe

-- |For each register remembers value
type Registers = [(Char,Int)]

-- |Represents instruction
type Instruction = [String]

-- |Current state 
--  current position
--  value of registers
--  value of last played sound
type State = (Int, Registers, Int)

inputString :: String
inputString = "set i 31\nset a 1\nmul p 17\njgz p p\nmul a 2\nadd i -1\njgz i -2\nadd a -1\nset i 127\nset p 735\nmul p 8505\nmod p a\nmul p 129749\nadd p 12345\nmod p a\nset b p\nmod b 10000\nsnd b\nadd i -1\njgz i -9\njgz a 3\nrcv b\njgz b -1\nset f 0\nset i 126\nrcv a\nrcv b\nset p a\nmul p -1\nadd p b\njgz p 4\nsnd a\nset a b\njgz 1 3\nsnd b\nset f 1\nadd i -1\njgz i -11\nsnd a\njgz f -16\njgz a -19\n"

-- |List of instructions
input :: [Instruction]
input = map words $ lines inputString

-- |State in the beginning
startState :: State
startState = (0,[],0)

-- |Returns value of the register
getValue :: Char -> Registers -> Int
getValue name val = fromMaybe 0 $ lookup name val

-- |Sets a value of register
setValue :: Char -> Int -> Registers -> Registers
setValue name val regs = (name, val) : removeFromRegs name regs

-- |When adding value, checks whether it's already there and deletes it
-- basically copied from day 08
removeFromRegs :: Char -> Registers -> Registers
removeFromRegs _ [] = []
removeFromRegs var ((x,i):xs) = if var == x then xs else (x,i):removeFromRegs var xs

-- |Performs one instruction
performInstruction :: State -> State
performInstruction (pos, regs, lp) = performInstruction' (pos, regs, lp) $ input!!pos

-- |Performs an instruction - gets instruction as an argument
performInstruction' :: State -> Instruction -> State
performInstruction' (pos, regs, lp) instr 
   | head instr == "snd" = (pos+1, regs, getNumOrVal (instr!!1) regs)
   | head instr == "set" = (pos+1, set (instr!!1) (instr!!2) regs, lp)
   | head instr == "add" = (pos+1, oper (instr!!1) (instr!!2) regs (+), lp)
   | head instr == "mul" = (pos+1, oper (instr!!1) (instr!!2) regs (*), lp)
   | head instr == "mod" = (pos+1, oper (instr!!1) (instr!!2) regs mod, lp)
   | head instr == "rcv" = if lp == 0 then (pos+1,regs,lp)
                                      else error $ "Value is " ++ show lp
   | head instr == "jgz" = if getNumOrVal (instr!!1) regs > 0 
                             then (pos + getNumOrVal (instr!!2) regs,regs, lp)
                             else (pos + 1, regs, lp)

-- |Performs set instruction
set :: String -> String -> Registers -> Registers
set first second regs = setValue var val regs
                    where var = head first
                          val = getNumOrVal second regs

-- |Performs instructions add, mul, mod
oper :: String -> String -> Registers -> (Int -> Int -> Int) -> Registers
oper first second regs f = setValue var val regs
                    where var = head first
                          val = getValue var regs `f` getNumOrVal second regs

-- |Some arguments can be values or register names
getNumOrVal :: String -> Registers -> Int
getNumOrVal s regs = if isNum $ head s then read s
                                       else getValue (head s) regs

-- |Starts running program for part 1. Errors out when finds an instruction
run :: [State]
run = iterate performInstruction startState

-- |Final state before first rcv instruction
finalState :: State
finalState = run !! 1373 --I tried running it, and found where it show the error

-- |Result is obtained from error
result1 = thd3 finalState

-- |Queue of messages - by program id
type Messages = [Int]

-- |Current state - part 2 version
--  position
--  values of registers
--  how many times have the program sent a value
type State2 = (Int, Registers, Int)

-- |State of both programs
--  messages for program 0
--  messages for program 1
--  states of both programs
type BothState = (Messages, Messages, State2, State2)

-- |Performs one instruction - part 2 version
performInstruction2 :: BothState -> BothState
performInstruction2 (mes0, mes1, st0, st1) = if w0 && w1 
                                             then error "Deadlock"
                                             else (mes0'',mes1'', st0', st1')
              where (mes0', mes1', st0',w0) = performInstruction2' mes0  mes1  st0 (input!!(fst3 st0))
                    (mes1'',mes0'',st1',w1) = performInstruction2' mes1' mes0' st1 (input!!(fst3 st1))

-- |Performs an instruction
--  arguments: program's messages
--             other program's messages
--             former state
-- result: (messages, other's messages, new state, bool)
-- Last argument is True when the program is waiting for an input
performInstruction2' :: Messages -> Messages -> State2 -> Instruction -> (Messages, Messages, State2, Bool)
performInstruction2' mesT mesO (pos, regs, num) instr
   | head instr == "snd" = (mesT, mesO ++ [getNumOrVal (instr!!1) regs], (pos+1, regs,num+1), False)
   | head instr == "set" = (mesT, mesO, (pos+1, set (instr!!1) (instr!!2) regs,num), False)
   | head instr == "add" = (mesT, mesO, (pos+1, oper (instr!!1) (instr!!2) regs (+),num), False)
   | head instr == "mul" = (mesT, mesO, (pos+1, oper (instr!!1) (instr!!2) regs (*),num), False)
   | head instr == "mod" = (mesT, mesO, (pos+1, oper (instr!!1) (instr!!2) regs mod,num), False)
   | head instr == "rcv" = if null mesT 
           then (mesT, mesO, (pos, regs,num), True)
           else (tail mesT, mesO, 
                 (pos+1,
                  set (instr!!1) (show . head $ mesT) regs,
                  num), 
                False)
   | head instr == "jgz" = if getNumOrVal (instr!!1) regs > 0 
                             then (mesT, mesO, (pos + getNumOrVal (instr!!2) regs,regs,num), False)
                             else (mesT, mesO, (pos + 1, regs,num), False)

-- |Start state for both programs
startState2 :: BothState
startState2 = ([],[],(0,[('p',0)],0),(0,[('p',1)],0))

-- |Starts running programs for part 2. Errors out when finds a deadlock.
run2 :: [BothState]
run2 = iterate performInstruction2 startState2

-- |Final state before deadlock
finalState2 :: BothState
finalState2 = run2 !! 71431 --I tried running it, and found where it show the error

result2 :: Int
result2 = thd3 . (\(_,_,_,x) -> x) $ finalState2
