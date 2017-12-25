-- Lot of this solution is the same as in Day 18

module Day23_coprocessor (result1, result2) where

import AoC2017
import Data.Maybe
import Data.Foldable (foldl')

-- |For each register remembers value
type Registers = [(Char,Int)]

-- |Represents instruction
type Instruction = [String]

-- |Current state 
--  current position
--  value of registers
--  number of muls
data State = Running Int Registers Int
           | Done Registers Int
           deriving (Eq, Show)

inputString :: String
inputString = "set b 84\nset c b\njnz a 2\njnz 1 5\nmul b 100\nsub b -100000\nset c b\nsub c -17000\nset f 1\nset d 2\nset e 2\nset g d\nmul g e\nsub g b\njnz g 2\nset f 0\nsub e -1\nset g e\nsub g b\njnz g -8\nsub d -1\nset g d\nsub g b\njnz g -13\njnz f 2\nsub h -1\nset g b\nsub g c\njnz g 2\njnz 1 3\nsub b -17\njnz 1 -23\n"

-- |List of instructions
input :: [Instruction]
input = map words $ lines inputString

-- |State in the beginning
startState :: State
startState = Running 0 [] 0

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
performInstruction (Running pos regs n) = 
        (\(Running npos nregs nn) -> if npos >= length input 
                                     then Done nregs nn 
                                     else Running npos nregs nn) $ 
               performInstruction' (Running pos regs n) $ input!!pos
performInstruction x = error $ "Last state was " ++ show x

-- |Performs an instruction - gets instruction as an argument
performInstruction' :: State -> Instruction -> State
performInstruction' (Running pos regs n) instr 
   | head instr == "set" = Running (pos+1) (set (instr!!1) (instr!!2) regs) n
   | head instr == "sub" = Running (pos+1) (oper (instr!!1) (instr!!2) regs (-)) n
   | head instr == "mul" = Running (pos+1) (oper (instr!!1) (instr!!2) regs (*)) (n+1)
   | head instr == "jnz" = if getNumOrVal (instr!!1) regs /= 0 
                             then Running (pos + getNumOrVal (instr!!2) regs) regs n
                             else Running (pos + 1) regs n

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

-- |Starts running program for part 1
run :: State
run = run' startState

run' :: State -> State
run' (Done regs n) = Done regs n
run' s = run' (performInstruction s)

-- |Number of multiplications
result1 :: Int
result1 = (\(Done _ i) -> i) run

-- |State in the beginning - part 2 version
startState2 :: State
startState2 = Running 0 [('a',1)] 0

-- |Starts running program for part 2
run2 :: State
run2 = run' startState2

-- |Running the part 2, but still not effective enough
run2Fold = foldl' (\state _ -> performInstruction state) startState2 [1..]

-- |What will be in h in the end
-- Analysis why is below
result2 :: Int
result2 = length [ 1 | b <- [108400,108417..125400], not $ isPrime b ]

{-
I probably would not start analyzing the code if I did not
read on Reddit that I need to do it to get this done.
----------------------------------------------------------
Here is my analyzation:
----------------------------------------------------------
The input is 

	set b 84
	set c b
	jnz a 2
	jnz 1 5
	mul b 100
	sub b -100000
	set c b
	sub c -17000
	set f 1
	set d 2
	set e 2
	set g d
	mul g e
	sub g b
	jnz g 2
	set f 0
	sub e -1
	set g e
	sub g b
	jnz g -8
	sub d -1
	set g d
	sub g b
	jnz g -13
	jnz f 2
	sub h -1
	set g b
	sub g c
	jnz g 2
	jnz 1 3
	sub b -17
	jnz 1 -23
	
----------------------------------------------------------
After doing some identification, I got this

	set b 84								b = 84
	set c b									c = 84

	jnz a 2									b=108400
		jnz 1 5								c=125400
			mul b 100
			sub b -100000
			set c b
			sub c -17000
											while true {
		set f 1									f=1
		set d 2									d=2
												do {
			set e 2									e=2
													do {
				set g d									
				mul g e								
				sub g b
				jnz g 2									if d*e-b == 0 
					set f 0									f=0
				sub e -1								e=e+1
				set g e									
				sub g b
				jnz g -8							} while e != b
			sub d -1								d=d+1
			set g d									
			sub g b
			jnz g -13							} while d != b
	jnz f 2										if f == 0
		sub h -1									h=h+1
	set g b										
	sub g c
	jnz g 2										if b==c return
		jnz 1 3 QUIT							
	sub b -17									b=b+17
	jnz 1 -23								}

----------------------------------------------------------
So the code is just

	b=108400
	c=125400
	while true {
		f=1
		d=2
		do {
			e=2
				do {
					if d*e == b 
						f=0
					e=e+1
				} while e != b
				d=d+1
		} while d != b
		if f == 0
			h=h+1
		if b==c return
		b=b+17
	}

----------------------------------------------------------
What it does?

The difference between b and c is 17000, therefore by 
adding 17 to b, I will get exactly to c. So I can change
it to for-cycle.
Another notable thing is what do the inner cycles and 
variable f do. This just looks whether there exist some
numbers 1 < d,e < b such that d*e=b. If yes, f will be set
to 0, otherwise to 1. Therefore, f is 1 if b is prime.
So, h is a number of the numbers b, that are prime.

----------------------------------------------------------
New code - some kind of pseudo code I guess

	for b = 108400 to 125400 by 17 {  
		if b is not prime
			h=h+1
	}

-}
