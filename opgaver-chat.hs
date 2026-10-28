{-
1. Multiples of both
Write multiples k, returning all positive numbers below k that are divisible by both 3 and 5.
-}
multiples :: Int -> [Int]
multiples k = [x | x <- [1..k-1], x `mod` 3 == 0 && x `mod` 5 == 0]

{-
2. Count occurrences
Write occurrences x xs, counting how many times x appears in xs.
-}
occurrences :: Eq a => a -> [a] -> Int
occurrences x xs = length [x' | x' <- xs, x' == x]

{-
Positions
Write positions x xs, returning the zero-based positions where x occurs. Use zip.
-}
positions :: Eq a => a -> [a] ->  [Int]
positions x xs = [i | (i, x') <- zip [0..] xs, x' == x]

{-
Ordered pairs
Write pairs n, returning all pairs (a,b) such that:
- Both numbers are between 1 and n.
- a < b.
- Their sum is even.
-}
pairs :: Integral a => a -> [(a, a)]
pairs n = [(a,b) | a <- [1..n], b <- [1..n], a < b && (a+b) `mod` 2 == 0]

{-
Flatten and filter
Write positiveValues xss, taking a list of lists and returning all strictly positive elements in their original order.
-}
positiveValues :: (Ord a, Num a) => [[a]] -> [a]
positiveValues xss = [x | xs <- xss, x <- xs, x > 0]



{-
Prime numbers
Write isprime n, returning whether n is prime. Numbers below 2 are not prime.

You may reuse your divisor function.
-}
divisor :: Int -> [Int]
divisor n = [y | y <- [1..n], n `mod` y == 0, n /= y]

isprime :: Int -> Bool
isprime n = length (divisor n) == 1

primes k = [x | x <- [2..k], isprime x == True]

{-
Perfect numbers
Reuse isperfect to write perfects k, returning all perfect numbers from 1 through k.
-}
isperfect :: Int -> Bool
isperfect n = n > 0 && sum ([x |x <- divisor n]) == n

perfects :: Int -> [Int]
perfects k = [x | x <- [1..k], isperfect x == True]

{-
Determine the types
Without using GHCi first, find the most general type of each function. Identify whether it is parametrically polymorphic or has type-class constraints.
-}
-- a
mysteryA xs = [y | (_,y) <- xs]

-- This has the type mysteryA :: [(a,b)] -> [b]
-- This is also parametrically polymorphic 

-- b
mysteryB x y = x == y

-- This has type mysteryB :: Eq a => a -> a -> Bool
-- This is adhoc-polymorphic type because of type class constrains

-- c
mysteryC xs = [x * 2 | x <- xs]

-- mysteryC :: Num a => [a] -> [a]
--is is also adhoc polymorphic 

-- d
mysteryD x y = (y, [x,x])

-- mysteryD :: a -> b -> (b, [a])
-- parametrically polymorphic 

-- e
mysteryE xs = [x | x <- xs, x > 0]

-- mysteryE :: (Num a, Ord a) => [a] -> [a]]
-- is adhoc polymorhpic

{-
Challenge: neighbouring elements
Write neighbours xs, pairing each element with the element immediately after it. Use zip and drop.
-}
neighbours xs = zip xs (drop 1 xs)
increasingPairs xs = [(x,y) | (x,y) <- neighbours xs, x < y]
