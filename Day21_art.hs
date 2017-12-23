module Day21_art (result1, result2) where

import AoC2017

-- |Image is stored as 2-dimensional list of chars
type Image = [[Char]]

-- |Rule for image patterning
type Rule = (Image, Image)

inputString :: String
inputString = "../.. => .##/.##/###\n#./.. => .../#.#/###\n##/.. => .##/.../.#.\n.#/#. => ###/.#./##.\n##/#. => .#./#../#.#\n##/## => .##/#.#/###\n.../.../... => ####/.##./####/.#..\n#../.../... => ..../..##/#.../.##.\n.#./.../... => #.#./##.#/#.../#.#.\n##./.../... => .#../.##./#.../....\n#.#/.../... => ###./..##/..##/##.#\n###/.../... => .###/#.##/..../....\n.#./#../... => ##.#/#..#/.##./...#\n##./#../... => ..../#..#/#.#./...#\n..#/#../... => #.##/.#../.#.#/###.\n#.#/#../... => ##../.#.#/...#/...#\n.##/#../... => ##.#/.##./..#./##.#\n###/#../... => ...#/####/..#./#...\n.../.#./... => ##.#/#.#./..##/.##.\n#../.#./... => .#.#/#.##/.##./....\n.#./.#./... => #..#/#.../.##./....\n##./.#./... => ###./###./..##/#..#\n#.#/.#./... => .###/...#/###./###.\n###/.#./... => ...#/..##/..#./#.##\n.#./##./... => .##./.#../...#/..#.\n##./##./... => .###/..#./.###/###.\n..#/##./... => .#.#/..#./..#./...#\n#.#/##./... => .#.#/##../#.../.##.\n.##/##./... => .##./...#/#.##/###.\n###/##./... => ...#/###./####/#.##\n.../#.#/... => #.#./#.../#.#./..#.\n#../#.#/... => ###./##../..#./.#..\n.#./#.#/... => #.../..##/#..#/#.#.\n##./#.#/... => #.#./.##./#..#/##.#\n#.#/#.#/... => #.##/.#.#/#..#/.#.#\n###/#.#/... => #.../##.#/###./....\n.../###/... => ..##/...#/##.#/###.\n#../###/... => .#.#/...#/#.##/.#..\n.#./###/... => ####/#.../..#./.#.#\n##./###/... => ..../####/#.##/#..#\n#.#/###/... => ####/..#./####/.#.#\n###/###/... => ..##/..../...#/.#..\n..#/.../#.. => .###/..##/.#.#/.##.\n#.#/.../#.. => #.##/#..#/.#.#/##.#\n.##/.../#.. => #.##/####/.#.#/..#.\n###/.../#.. => ##../##.#/..../##..\n.##/#../#.. => ...#/####/..##/.##.\n###/#../#.. => ..#./...#/#.../##.#\n..#/.#./#.. => #..#/##.#/..##/#..#\n#.#/.#./#.. => ..../.###/#..#/..##\n.##/.#./#.. => ..#./...#/..##/...#\n###/.#./#.. => ...#/..../##.#/....\n.##/##./#.. => .#../..##/...#/.#.#\n###/##./#.. => .###/#.#./####/#.#.\n#../..#/#.. => .###/##.#/##../##..\n.#./..#/#.. => ##../.#../###./##.#\n##./..#/#.. => #..#/####/####/..##\n#.#/..#/#.. => ..##/..../###./..##\n.##/..#/#.. => ..##/.#.#/.#../.#..\n###/..#/#.. => ...#/.###/.###/.#.#\n#../#.#/#.. => ##../##../##.#/.##.\n.#./#.#/#.. => ...#/.##./.#.#/#...\n##./#.#/#.. => .##./.#../.#../#...\n..#/#.#/#.. => ..##/##.#/####/###.\n#.#/#.#/#.. => ..../.###/#.../#..#\n.##/#.#/#.. => ..#./#.#./.#../...#\n###/#.#/#.. => ##.#/#.../##.#/.##.\n#../.##/#.. => ..../#.../..#./####\n.#./.##/#.. => #..#/.#../#.#./..##\n##./.##/#.. => .###/..##/###./....\n#.#/.##/#.. => .###/.##./.###/#.##\n.##/.##/#.. => #.##/###./.##./...#\n###/.##/#.. => ...#/#.##/.##./#.#.\n#../###/#.. => #..#/.###/.###/#.#.\n.#./###/#.. => ..#./#.#./..../...#\n##./###/#.. => ..##/##../#..#/....\n..#/###/#.. => ..##/.#../.#../###.\n#.#/###/#.. => ..#./.###/..../...#\n.##/###/#.. => .##./###./#.../#.##\n###/###/#.. => ##.#/..../.##./##.#\n.#./#.#/.#. => .##./.#.#/####/....\n##./#.#/.#. => ##.#/#.##/####/.#..\n#.#/#.#/.#. => ####/.##./##.#/...#\n###/#.#/.#. => #..#/#.##/.##./###.\n.#./###/.#. => .#../..../.##./##.#\n##./###/.#. => ##.#/.#../#.../.###\n#.#/###/.#. => ###./###./.#../###.\n###/###/.#. => #..#/#.../#..#/.#.#\n#.#/..#/##. => #..#/#.../##../###.\n###/..#/##. => #.../.#../.###/#...\n.##/#.#/##. => .#.#/.##./.#../##.#\n###/#.#/##. => #.../..../##../.###\n#.#/.##/##. => .#.#/##../.###/#.#.\n###/.##/##. => ###./..#./##.#/.###\n.##/###/##. => ..#./.#.#/##.#/#.#.\n###/###/##. => ##../.#.#/#..#/.#.#\n#.#/.../#.# => ##../###./..#./##.#\n###/.../#.# => .#../##../..#./##.#\n###/#../#.# => ###./#..#/####/....\n#.#/.#./#.# => .###/..../.###/##.#\n###/.#./#.# => ###./.###/..##/.#.#\n###/##./#.# => ..#./..##/#..#/#.##\n#.#/#.#/#.# => .#.#/.#../.#.#/#.##\n###/#.#/#.# => .###/#.../##../.###\n#.#/###/#.# => .#../...#/..../...#\n###/###/#.# => #..#/##.#/..#./#...\n###/#.#/### => .###/.#.#/..#./####\n###/###/### => ##.#/..##/.#../..##\n"

-- |A set of rules from the input
rules :: [Rule]
rules =  map (\s -> (lines. repl . head $ s, lines . repl . last $ s)) $ map words . lines $ inputString
      where repl = replace '/' '\n'

-- |Replaces all occurences of x in list with y
replace :: Eq a => a -> a -> [a] -> [a]
replace x y [] = []
replace x y (z:xs) = if x == z then y : replace x y xs else z : replace x y xs

-- |Image to begin with
start :: Image
start = [".#.","..#","###"]

-- |Takes an image and draws it
draw :: Image -> IO ()
draw img = putStr . concat $ map (++ "\n") img

-- |Flips image
flipImg :: Image -> Image
flipImg = map reverse

-- |Rotates image by 90 degrees
rotateImg :: Image -> Image
rotateImg img = flipImg . rotateImg' $ img

rotateImg' :: Image -> Image
rotateImg' ([]:_) = []
rotateImg' img = map head img : rotateImg' (map tail img)

-- |First argument is image, second argument is pattern and
-- this returns true if the image (can be flipped and rotated)
-- matches the pattern
patternMatch :: Image -> Image -> Bool
patternMatch img pat = 
   img == pat                                               ||
   flipImg img == pat                                       ||
   rotateImg img == pat                                     ||
   (flipImg . rotateImg) img == pat                         ||
   (rotateImg . rotateImg) img == pat                       ||
   (flipImg . rotateImg . rotateImg) img == pat             ||
   (rotateImg . rotateImg . rotateImg) img == pat           ||
   (flipImg . rotateImg . rotateImg . rotateImg) img == pat

-- |If image size is divisible by 2, then breaks the image into 2x2
-- squares. Otherwise, breaks the image into 3x3 squares.
breakImg :: Image -> [[Image]]
breakImg img = if even $ length img then break22 img
                                    else break33 img

-- |Breaks the image into 2x2 squares
break22 :: Image -> [[Image]]
break22 [] = []
break22 (x:y:xs) = break22' x y : break22 xs

break22' :: [Char] -> [Char] -> [Image]
break22' [] [] = []
break22' (x:x1:xs) (y:y1:ys) = [x:[x1],y:[y1]] : break22' xs ys

-- |Breaks the image into 3x3 squares
break33 :: Image -> [[Image]]
break33 [] = []
break33 (x:y:z:xs) = break33' x y z : break33 xs

break33' :: [Char] -> [Char] -> [Char] -> [Image]
break33' [] [] [] = []
break33' (x:x1:x2:xs) (y:y1:y2:ys) (z:z1:z2:zs) = [x:x1:[x2],y:y1:[y2],z:z1:[z2]] : break33' xs ys zs

-- |Finds a pattern for this image and replaces it
replacePattern :: [Rule] -> Image -> Image
replacePattern [] _  = error "Could not find pattern"
replacePattern ((a,b):rs) img = if patternMatch img a then b else replacePattern rs img

-- |Replaces pattern for all images
-- this assumes all rules are saved in function rules
replacePatterns :: [[Image]] -> [[Image]]
replacePatterns = map (map $replacePattern rules)

-- |Connects broken images back into one image
connectImg :: [[Image]] -> Image
connectImg = connect'

connect' :: [[Image]] -> Image
connect' [] = []
connect' (x:xs) = connect'' x ++ connect' xs

connect'' :: [Image] -> Image
connect'' ([]:_) = []
connect'' imgs = (concat . map head) imgs : connect'' (map tail imgs)

-- |Performs an iteration of the algorithm
iteration :: Image -> Image
iteration = connectImg . replacePatterns . breakImg

-- |Number of pixels in image which are on (#)
numberOn :: Image -> Int
numberOn img = sum $ map numberOn' img

numberOn' :: [Char] -> Int
numberOn' [] = 0
numberOn' ('#':xs) = 1 + numberOn' xs
numberOn' (_:xs) = numberOn' xs

-- |How many pixels are on after 5 iterations
result1 = numberOn . iterateN 5 iteration $ start

-- |How many pixels are on after 18 iterations
-- took about 2-3 minutes
result2 = numberOn . iterateN 18 iteration $ start
