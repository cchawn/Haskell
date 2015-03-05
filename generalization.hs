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

-- 10.8 type and define a function twice
-- takes function from ints to ints, and an
-- input int, output is function applied
-- to input twice
twice :: (Int -> Int) -> Int -> Int
twice f x = f (f x)

double :: Int -> Int
double x = x * 2

-- 10.9 type of and define a function iter
-- iter n f x = f (f... (f x)...))
iter :: Int -> (a -> a) -> a -> a
iter 0 _ x = x
iter n f x = iter (n - 1) f (f x)

-- 10.10 using iter and double, define a function
-- on input n returns 2^n; 1 * 2 n times
pow2 :: Int -> Int
pow2 n = iter n double 1

-- 10.13 define sum of squares from 1 .. n 
-- using map and foldr
sumSquares2 :: Int -> Int
sumSquares2 n = foldr (+) 0 (map square [1..n])
	where square x = x * x

-- 10.14 define sum sqaures for positive [int]
sumSquaresPos :: [Int] -> Int
sumSquaresPos lis = foldr (+) 0 (map square (filter positive lis))
	where
		positive x = x > 0
		square x = x * x

-- 10.18 filterFirst :: (a -> Bool) -> [a] -> [a]
-- filterFirst p xs removes first element of xs
-- that does not have property p
filterFirst :: (a -> Bool) -> [a] -> [a]
filterFirst p [] = []
filterFirst p (x:xs)
	| p x == True = x : (filterFirst p xs)
	| otherwise = xs

-- 11.2 if id is polymorphic identity funciton
-- defined by id x = x explain behaviour of:
-- (id.f)	(f.id)	id f
-- f is of type Int -> Bool, at what instance
-- of its most general type a -> a is id used in each case?
-- what type does f have if f id is properly typed?
{-
-}

-- 11.3 define a function composeList that composes
-- a list of functions into a single function
composeList :: [a -> a] -> a -> a
composeList [] = id
composeList (f:fs) = f . (composeList fs)

-- 11.7 using lamda abstraction, "not", and "elem"
-- descrie a function Char -> Bool, which is True
-- only on non-whitespace characters
notWhiteSpace :: Char -> Bool
notWhiteSpace = \c -> not (elem c " \t\n")

-- 11.8 define a function total, so that total f
-- is the funcitn which at value n gives the total
-- f 0 + f 1 + ... + f n
-- use built in funcitons instead of recursion
total :: (Int -> Int) -> (Int -> Int)
total f n = sum (map f [0..n])
-- using lambda: total f = \n sum (map f [0..n])

-- 11.9 function of type "a -> b -> c" write a lambda
-- abstraction that describes function "b -> a -> c"
-- behaves like f but takes arguments in other order
flip1 f = \x y -> f y x

-- 11.10 give a definition which reverses order
-- of function arguments (using above solution)
flip2 :: (a -> b -> c) -> (b -> a -> c)
flip2 f x y = f y x

-- 11.12 find operator sections sec1 and sec2 so
-- map sec1 . filter sec2
-- has same effect as 
-- filter (>0) . map (+1)