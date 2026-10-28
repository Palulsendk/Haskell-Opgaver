-- Defining functions

-- 1. Using library functions, define a function halve :: [a] -> ([a], [a]) that splits an evenlengthed list into two halves. For example
-- > halve [1,2,3,4,5,6] --> ([1,2,3],[4,5,6])
halve :: [a] -> ([a],[a])
halve xs = if modular then (take divider xs, drop divider xs) else (xs, [])
        where 
            modular = length xs `mod` 2 == 0
            divider = length xs `div` 2
    

-- 2.  Define a function third :: [a] -> a that returns the third element in a list that contains at least 
-- this many elements using:
-- a:
third :: [a] -> a
third xs = head (tail (tail xs))

-- b:
third' :: [a] -> a
third' xs = xs !! 2

-- c:
third''(_:_:x:_) = x

{-
Consider a function safetail :: [a] -> [a] that behaves in the same way as tail except that
it maps the empty list to itself rather than producing an error. Using tail and the function null ::
[a] -> Bool that decides if a list is empty or not, define safetail using:
-}

-- a: 
safetail :: [a] -> [a]

safetail xs = 
    if null xs  
    then [] 
    else tail xs 
    where 
        null :: [a] -> Bool
        null xs = length xs == 0 

-- b:
safetail' :: [a] -> [a]
safetail' xs 
    | null xs = []
    | otherwise = tail xs    
    where 
        null :: [a] -> Bool
        null xs = length xs == 0 

-- c:
safetail'' :: [a] -> [a]
safetail'' [] = []
safetail'' (_:xs) = xs 


{-
In a similar way to && in section 4.4, show how the disjunction operator || can be defined in four
different ways using pattern matching
-}
(||) :: Bool -> Bool -> Bool
True || True = True
True || False = True
False ||True = False
False || False = False


{-
Without using any other library functions or operators, show how the meaning of the
following pattern matching definition for logical conjunction && can be formalised using
conditional expressions:
True && True = True
_ && _ = False
Hint: use two nested conditional expressions.
-}
logicalConjuntion :: Bool -> Bool -> Bool
logicalConjuntion a b = 
    if a
        then if b
            then True
            else False
    else False


{-
Do the same for the following alternative definition, and note the difference in the number of
conditional expressions that are required:
-}
logicalConjuntion' a b =
    if a == True
        then if b == True
            then True
            else False
    else False

{-
Show how the meaning of the following curried function definition can be formalised in terms of
lambda expressions:

mult :: Int -> Int -> Int -> Int
mult x y z = x * y * z
-}
mult :: Int -> (Int -> (Int -> Int))
mult = \x -> (\y -> (\z -> x*y*z))

{-
You need to write two functions:
1. luhnDouble :: Int -> Int
   Takes one digit, doubles it, and checks the result:
   - If the doubled value is greater than 9, subtract 9.
   - Otherwise, return the doubled value.
2. luhn :: Int -> Int -> Int -> Int -> Bool
   Takes four separate digits and checks whether they form a valid number:
   - Apply luhnDouble to the first and third digits. These are every other digit counting left from the second-last.
   - Keep the second and fourth digits unchanged.
   - Add the four resulting values.
   - Use mod to check whether the total is divisible by 10, returning True or False.
Start with luhnDouble and test it using the examples. Once it works, use it inside luhn.
-}
luhn :: Int -> Int -> Int -> Int -> Bool
luhn x y z w = modular (luhnDouble x) y (luhnDouble z) w
    where 
        luhnDouble :: Int -> Int
        luhnDouble x = if x * 2 > 9
            then x * 2 - 9
            else x * 2
        modular :: Int -> (Int -> (Int -> (Int -> Bool)))
        modular = \x -> (\y -> (\z -> \w -> (x+y+z+w) `mod` 2 == 0))


{-
### List Comprehension ###   
-}
{-
Using a list comprehension, give an expression that calculates the sum 12 + 22 + ... 1002 of
the first one hundred integer squares
-}
hundredInteegersSum x = sum [x*2 | x <- [1..100]]

{-
Suppose that a coordinate grid of size m x n is given
by the list of all pairs (x,y) of integers such that 0 <= x <= m
and 0 <= y <= n. Using a list comprehension, define
a function grid :: Int -> Int -> Int [(Int, Int)] that
reutnrs a coordinate grid of a given size 
for example:
grid 1 2 
[(0,0),(0,1),(0,2),(1,0),(1,1),(1,2)]
-}
grid :: Int -> Int -> [(Int, Int)]
grid x y = [(x,y) | x <- [0..x], y <- [0..y]]

{-
Using a list comprehension and the function grid above, define a function square ::
Int -> [(Int,Int)] that returns a coordinate square of size n, excluding the diagonal from (0,
0) to (n, n). For example:
-}
square :: Int -> [(Int, Int)]
square x = [(x,y) | (x,y) <- grid x x, x /= y]


{-
 In a similar way to the function length, show how the library function replicate
:: Int -> a -> [a] that produces a list of identical elements can be defined using a list
comprehension. For example:
> replicate 3 True
[True,True,True]

-}
replicateTrue :: Int -> a -> [a]
replicateTrue n y = [y | _ <- [1..n]]

{-
A triple (x, y, z) of positive integers is Pythagorean if it satisfies the equation x2 + y2 = z2. Using a
list comprehension with three generators, define a function pyths :: Int -> [(Int,Int,Int)]
that returns the list of all such triples whose components are at most a given limit. For example:
> pyths 10
[(3,4,5),(4,3,5),(6,8,10),(8,6,10)]
-}
pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x,y,z) | x <- [1..n], y <- [1..n], z <- [1..n], x^2 + y^2 == z^2]

{-
A positive integer is perfect if it equals the sum of all of its factors, excluding the number itself.
Using a list comprehension and the function factors, define a function perfects :: Int
-> [Int] that returns the list of all perfect numbers up to a given limit. For example:
> perfects 500
[6,28,496]
-}

factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], x == sum [f | f <- factors x, f /= x]]