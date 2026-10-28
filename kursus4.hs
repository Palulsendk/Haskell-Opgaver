split :: [(a,b)] -> [(b,a)]
split xs = [(b,a) | (a,b) <- xs]


-- Splits a list at the n-th element and return a 2-tuple
splitAt_func :: Int -> [a] -> ([a], [a])
splitAt_func n xs = (take n xs, drop n xs)


-- If the list is even, then we split otherwise, it should just return the list.
-- Could be fun to also make this recursive later
isEven_split :: [a] -> Either ([a], [a]) [a]
isEven_split xs = 
    if modular 
    then Left (take division xs, drop division xs) 
    else Right xs
    where
        modular = length xs `mod` 2 == 0
        division = length xs `div` 2


-- Same as above, but with guards instead
isEven_splitGuards :: [a] -> Either ([a], [a]) [a]
isEven_splitGuards xs | modular = Left (take division xs, drop division xs)
                       | not modular = Right xs
    where
        modular = length xs `mod` 2 == 0
        division = length xs `div` 2

-- lamda function
double = \x -> x * 2


-- List Comprehension
doubleList :: Num a => [a] -> [a]
doubleList xs = [x^2|x <- xs]

combineTwoLists xs ys = [(x,y)| x <- xs, y <- ys]

firsts ps = [x| (x, _) <- ps]

testLength xs = sum[1 | _ <- xs]

--sortEvenIntegers :: Integral a => [a] -> [a]

sortEvenIntegers :: Integral a => [a] -> ([a], [a])
sortEvenIntegers xs = ([x | x <- xs, even x], [x |x <- xs, odd x])

-- Created pairs of a list, and check if that list is sorted correctly
-- We can use 'and' which takes a list of bools and decides the outcome.
-- If one is false in a sequence of true, than the result is false and wise versa.
pairs xs = zip xs (tail xs)
sortedList xs = and [ x <= y | (x,y) <- pairs xs]

-- Find where in a list some value is at?
-- The smart thing here is that we use zip and add 
positions :: Eq a => a -> [a] -> [Int] 
positions x xs = [i | (i, x') <- zip [0..] xs, x == x']


lowers xs = length [x | x <- xs, x >= 'a' && 'z' >= x]
