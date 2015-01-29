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

-- 5.2
-- 5.16
-- 5.17
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