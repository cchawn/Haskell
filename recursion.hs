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

-- 4.20 The integer square root of a positive integer
-- n is the largest int whose square is <= to n.
recursiveSqrtCount :: Int -> Int -> Int
recursiveSqrtCount n x
	| (x*x) <= n = x
	| otherwise = recursiveSqrtCount n (x-1)

recursiveSqrt :: Int -> Int
recursiveSqrt n
	| n < 1 = 0
	| n == 1 = 1
	| otherwise = recursiveSqrtCount n n

-- 4.21 give a recursive definition of a function
-- which on input n returns the max of the values
-- f 0, f1, ... f n (where f is some other funciton)
maxFCount :: (Int -> Int) -> Int -> Int -> Int -> Int
maxFCount f n x high
	| x == 0 = high
	| (f x) > high = maxFCount f n x (f x)
	| otherwise = maxFCount f n (x-1) high

maxF :: (Int -> Int) -> Int -> Int
maxF f n
	| n==1 = f n
	| otherwise = maxFCount f n n 0

func 0 = 1
func 1 = 44
func 2 = 17
func _ = 0

-- 4.22 Given a function f, give a recursive definition
-- of a funciton which on an input n returns True if one
-- or more of the values of f 0, ..., f n is zero
zeroFCount :: (Int -> Int) -> Int -> Int -> Int -> Int
zeroFCount f n x count
	| x == 0 = count
	| (f x) == 0 = zeroFCount f n (x-1) (count+1)
	| otherwise = zeroFCount f n (x-1) count

zeroF :: (Int -> Int) -> Int -> Bool
zeroF f n
	| n < 0 = error "you fucked up"
	| (zeroFCount f n n 0) > 0 = True
	| otherwise = False

-- 4.23 Give a definition of regions which uses sumFun
-- instead of being recursive
regions :: Int -> Int
regions n
	| n == 0 = 1
	| n > 0 = regions (n-1) + n

-- doing this later

-- 4.31 Give a recursive definition of a function to
-- find the hightest common factor of two positive ints
highestFacCount :: Int -> Int -> Int -> Int -> Int
highestFacCount n m x high
	| x == 1 = high
	| ((mod n x) == 0) && ((mod m x) == 0) && (x > high) = highestFacCount n m (x-1) x
	| otherwise = highestFacCount n m (x-1) high

highestFac :: Int -> Int -> Int
highestFac n m
	| (n < m) && ((mod m n) == 0) = n
	| (n > m) && ((mod n m) == 0) = m
	| (n < m) = highestFacCount n m n 0
	| otherwise = highestFacCount n m m 0

-- 4.32 give a recursive function to compute 2^n
pow2 :: Int -> Int
pow2 n
	| n == 0 = 1
	| n == 1 = 2
	| (mod n 2) == 0 = ((pow2 (floor ((fromIntegral n) / 2)))*(pow2 (floor ((fromIntegral n) / 2))))
	| otherwise = ((pow2 (floor ((fromIntegral n) / 2)))*(pow2 (floor ((fromIntegral n) / 2))))*2
