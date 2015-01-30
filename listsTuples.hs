module ListsTuples where
-- 5.1 Define a function which returns the
-- max of two integers, togehter with the number
-- of times it occurs, using this function define
-- another function which does same for 3 arguments
maxOccurs :: Int -> Int -> (Int, Int)
maxOccurs x y
	| x == y = (x, 2)
	| x < y = (y, 1)
	| otherwise = (x, 1)

maxThreeOccurs :: Int -> Int -> Int -> (Int, Int)
maxThreeOccurs x y z
	| z == maxTwo = (z, occursTwo + 1)
	| z > maxTwo = (z, 1)
	| otherwise = (maxTwo, occursTwo)
	where (maxTwo, occursTwo) = maxOccurs x y

-- 5.2 Define a function that puts the elements of 
-- a triple into ascending order.
-- from text (page 39)
maxThree :: Int -> Int -> Int -> Int
maxThree x y z
	| x >= y && x >= z = x
	| y >= z = y
	| otherwise = z
  
-- from solution to 3.11
minThree :: Int -> Int -> Int -> Int
minThree a b c
	| a <= b && a <= c = a
	| b <= c = b
	| otherwise = c

-- from text (page 57)
middleNumber :: Int -> Int -> Int -> Int
middleNumber x y z
	| between y x z = x
	| between x y z = y
	| otherwise = z

-- helper function for middleNumber
between a b c 
	| a <= c = a <= b && b <= c
	| otherwise = a >= b && b >= c

orderThree :: (Int, Int, Int) -> (Int, Int, Int)
orderThree (x, y, z) =
	((minThree x y z), (middleNumber x y z), (maxThree x y z))

-- 5.16 How many items does the list [2,3] contain?
-- how many does [[2,3]] contain? What is the type?
{-
	[2,3] is a list containing 2 integers
	[[2,3]] is a list of Int lists, with one element
-}

-- 5.17 What is the result of evaluating [2..4]?
-- What about [2,7..4]?
{-
	[2,3,4]
	[2]
-}

-- 5.18 Define a function which doubles all
-- elements of a list of integers.
doubleAll :: [Int] -> [Int]
doubleAll [] = []
doubleAll (x:xs) = (2*x) : (doubleAll xs)

-- 5.19 Define a function which converts all small
-- letters in a String to capitals, leaving other
-- characters unchanged. Then write similar function,
-- but it removes all non-letters.
toUpper :: Char -> Char
toUpper ch
	| ('a' <= ch) && (ch <= 'z') = toEnum (fromEnum ch + (fromEnum 'A' - fromEnum 'a'))
	| otherwise = ch

capitalize :: String -> String
capitalize "" = ""
capitalize (c:cs) = (toUpper c) : (capitalize cs)

capitalizeLetters :: String -> String
capitalizeLetters "" = ""
capitalizeLetters (c:cs)
	| ('A' <= c) && (c <= 'z') = (toUpper c) : (capitalizeLetters cs)
	| otherwise = capitalizeLetters cs

-- 5.20 Define a function which returns a list of divisors
-- of a positive integer (and the empty list for other inputs)
-- using divisors write a function isPrime
divisors :: Int -> [Int]
divisors n
	| n <= 0 = []
	| otherwise = [x | x <- [1..n], (mod n x) == 0]

isPrime :: Int -> Bool
isPrime n
	| n < 1 = False
	| otherwise = True -- haha NOPE not doing this

-- 5.21 Define a funtion which picks out all occurences
-- of an integer n in a list
-- using this, define a function wich returns True if
-- element is in list, and False otherwise
matches :: Int -> [Int] -> [Int]
matches n [] = []
matches n (x:xs)
	| n == x = n : (matches n xs)
	| otherwise = matches n xs

element :: Int -> [Int] -> Bool
element n [] = False
element n (x:xs) = n == x || element n xs

-- 7.5
-- 7.6
-- 7.8
-- 7.9
-- 7.21
-- 7.23
-- 7.25
-- 7.28
-- 7.33
-- 7.34