-- 1
idhead :: Eq a => [(a, a)] -> Bool
idhead ((x,y):_) = x == y

-- 2
pyt :: Int -> [(Int, Int, Int)]
pyt x = [(a, b, c) | a <- [0..x], 
                    b <- [0..x], 
                    c <- [0..x], 
                    a <= b && b < c &&
                    a^2+b^2==c^2]

-- 3
bighead xs = sum [1 | (_, x) <- zip [0..] xs, x > (head xs)]

-- 4
plonk :: Int -> (Int -> (Int -> Int))
plonk = \x -> (\y -> (\z -> x+y+z))


-- 5 
divisor :: Int -> [Int]
divisor n = [y | y <- [1..n], n `mod` y == 0, n /= y]

isperfect :: Int -> Bool
isperfect n = sum ([x |x <- divisor n]) == n

-- a
sevens :: Int -> [Int]
sevens k = [x | x <- [1..k-1], x `mod` 7 == 0]

-- b
flop :: [(a,b)] -> [(b,a)]
flop xs = [(y,x) | (x,y) <- xs]

-- c
dupli xs = concat[[x,x] | x <- xs ]

-- d s
sums m n = [x+y | x <- [1..m], y <- [1..n]]
sums' m n = concat [[x+y | y <- [1..n]] | x <- [1..m]]

