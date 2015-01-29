module Basics where

-- 3.8 Guess what "mystery" does
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

-- 3.16 Define a function to convert lower
-- case letters to upper case
toUpper :: Char -> Char
toUpper ch = toEnum (fromEnum ch + (fromEnum 'A' - fromEnum 'a'))

-- 3.17 Define a function to convert a digit '8'
-- to its value 8. Non-digit values should be 0
isDigit :: Char -> Bool
isDigit ch = ('0' <= ch) && (ch <= '9')

charToNum :: Char -> Int
charToNum ch
	| not (isDigit ch) = 0
	| otherwise = 9 - (fromEnum '9' - fromEnum ch)

-- 3.18 Define a function which takes three
-- strings and returns a single string which
-- with each string on a seperate line
onThreeLines :: String -> String -> String -> String
onThreeLines a b c = a ++"\n" ++ b ++ "\n" ++ c

-- 3.19 Define a function which converts a digit to
-- its representation in Roman numerals
romanDigit :: Char -> String
romanDigit ch
	| not (isDigit ch) || ch == '0' = "error"
	| ch == '1' = "I"
	| ch == '2' = "II"
	| ch == '3' = "III"
	-- note: there is probably a better way to do this

-- 3.20 Define a function to return the average
-- of three integers, then using this function
-- define a function that returns how many inputs
-- are larger than their average value
averageThree :: Int -> Int -> Int -> Float
averageThree a b c = (fromIntegral (a + b + c)) / 3

howManyAboveAverage :: Int -> Int -> Int -> Int
howManyAboveAverage a b c = 0
-- do this later!

-- 3.22 Write a function that given the coefficients
-- of a quadratic a b c, will return how many roots
-- the equation has (assumes non-degenerate)
numberNDroots :: Float -> Float -> Float -> Int
numberNDroots a b c
	| (b*b) > (4.0*a*c) = 2
	| (b*b) == (4.0*a*c) = 1
	| (b*b) < (4.0*a*c) = 0


-- 3.23 Using "numberNDroots", write a function
-- that given the coefficients of the quadratic
-- will return how many roots the equation has
numberRoots :: Float -> Float -> Float -> Int
numberRoots a b c
	| not (a == 0.0) = numberNDroots a b c
	| b /= 0.0 = 1
	| (b == 0.0) && (c /= 0.0) = 0
	| otherwise = 3