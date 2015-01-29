module Basics where

-- 3.8
mystery :: Int -> Int -> Int -> Bool
mystery m n p = not ((m==n) && (n==p))

-- 3.9 Define a function "threeDifferent"
-- so that result is True only if
-- all three numbers are different
threeDifferent :: Int -> Int -> Int -> Bool
threeDifferent m n p = not ((m==n) || (n==p) || (m==p))

-- 3.10 Define a function "fourEqual"
-- modelled after "threeEqual"
-- using "threeEqual"
threeEqual :: Int -> Int -> Int -> Bool
threeEqual m n p = (m==n) && (n==p)

fourEqual :: Int -> Int -> Int -> Int -> Bool
--fourEqual a b c d = (a==b) && (a==c) && (a==d)
fourEqual a b c d = (threeEqual a b c) && (c==d)

-- 3.14 Define functions that calculate
-- the minimum of two and three integers
myMin :: Int -> Int -> Int
myMin a b
	| a < b = a
	| otherwise = b

myMinThree :: Int -> Int -> Int -> Int
myMinThree a b c
	| (myMin a b) < c = (myMin a b)
	| otherwise = c

