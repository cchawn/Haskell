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

-- 5.18
-- 5.19
-- 5.20
-- 5.21

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