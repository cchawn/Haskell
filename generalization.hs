module Generalization where

-- 10.2 Define length function using map and sum
length :: [a] -> Int
length lis = sum (map one lis)
	where one x = 1 -- function always returns one

-- 10.3 Given the function
-- addUp ns = filter greaterOne (map addOne ns)
-- 		where
--		greaterOne n 	= n>1
--		addOne n 		= n + 1
-- redifine it using filter before map
addUp ns = map addOne (filter geqOne ns)
	where
		geqOne n = n >= 1
		addOne n = n + 1

-- 10.6 Define functions to take a list of ints ns
-- return list of squares
-- return sum of squares of itmes in ns
-- check if all greater than zero
squares :: [Int] -> [Int]
squares ns = map square ns
	where square x = x * x

sumSquares :: [Int] -> Int
sumSquares ns = sum (squares ns)

allPositive :: [Int] -> Bool
allPositive ns = and (map positive ns)
	where positive x = x > 0