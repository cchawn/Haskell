module Recursion where

-- 4.17 Define a function which when given
-- natural numbers m and n returns the product:
-- m*(m+1)*...*(n-1)*n
rangeProduct :: Int -> Int -> Int
rangeProduct m n
	| m > n = 0
	| m == n = m
	| otherwise = m * (rangeProduct (m+1) n)

-- 4.18 Define a function for factorial that
-- uses rangeProduct
fac :: Int -> Int
fac n = rangeProduct 1 n

-- 4.19 Using addition, give a recursive definition
-- of multiplication of natural numbers
recursiveMult :: Int -> Int -> Int
recursiveMult a b
	| (a == 0) || (b == 0) = 0
	| a == 1 = b
	| b == 1 = a
	| otherwise = b + (recursiveMult (a - 1) b)

-- 4.20

-- 4.21

-- 4.22

-- 4.23

-- 4.31

-- 4.32