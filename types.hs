module Types where
-- 13.17 predict type erors with following:
-- f :: Bool -> Int
f n = 37 + n -- expects n to be Bool, can't do (Bool + Int)
f True = 34 -- good

-- g :: Int -> Bool
g 0 = 37 -- 37 should be a Bool
g n = True

-- h :: Int -> Bool
h x
	| x > 0 = True
	| otherwise = 37 -- should be Bool

-- k :: Int -> Int
k x = 34
k 0 = 35

-- 13.18 do the following pairs of types unify
-- if so give a unifier for them
-- unification = most general common instance of two type expressions
(Int -> b) (a -> Bool) -- unifies: Int -> Bool
(Int, a, a) (a, a, [Bool]) -- cannot be unified
-- first element of triple, a must be Int
-- last element of triple, a must also be [Bool]
-- a cannot be both ---> cannot be unified

-- 13.19 show that we can unify (a, [a]) with
-- (b, c) to give (Bool, [Bool])
{-
	type b can be anthing, type c can be anything
	type a must be Bool, and type [a] is [Bool]
-}

-- 13.22
-- 13.24
-- 13.26