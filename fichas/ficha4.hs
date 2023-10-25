import Data.Char

--1
digitAlpha :: String -> (String,String)
digitAlpha [] = ([],[])
digitAlpha (h:t) | isDigit h = (a,h:b)
                 | isAlpha h = (h:a,b)
                 | otherwise = (a,b)
    where (a,b) = digitAlpha t

--2
nzp :: [Int] -> (Int,Int,Int)
nzp [] = (0,0,0)
nzp (h:t) | h > 0 = (a,b,c+1)
          | h == 0 = (a,b+1,c)
          | otherwise = (a+1,b,c)
    where (a,b,c) = nzp t

--3
divMod' :: Integral a => a -> a -> (a, a)
divMod' a b | a - b >= 0 = (1+x,y)
            | otherwise = (0,a)
    where (x,y) = divMod' (a-b) b

--4
fromDigits :: [Int] -> Int
fromDigits [] = 0
fromDigits l = aux1 l 0

aux1 :: [Int] -> Int -> Int
aux1 [] acc = acc
aux1 (h:t) acc = aux1 t (h*10^(length t)+acc)

--5
inits' :: [a] -> [[a]]
inits' [] = [[]]
inits' l = inits' (init l) ++ [l]

maxSumInit :: (Num a, Ord a) => [a] -> a
maxSumInit l = aux2 l 0

aux2 :: (Num a, Ord a) => [a] -> a -> a
aux2 [] acc = acc
aux2 l acc | acc < sum l = aux2 (init l) (sum l)
           | otherwise = aux2 (init l) acc

--6
fib :: Int -> Int
fib n = aux3 n 0 1

aux3 :: Int -> Int -> Int -> Int
aux3 0 acc _ = acc
aux3 n acc1 acc2 = aux3 (n-1) (acc1+acc2) acc1

--7
intToStr :: Integer -> String
intToStr n | n < 0 = "-" ++ intToStr n
           | n == 0 = "0"
           | otherwise = aux4 n ""

aux4 :: Integer -> String -> String
aux4 0 acc = acc
aux4 n acc = aux4 (n `div` 10) ([intToDigit(fromIntegral(n `mod` 10))] ++ acc)

--8
--a
--[x | x <- [1..20], mod x 2 == 0, mod x 3 == 0] = [6,12,18]

--b
--[x | x <- [y | y <- [1..20], mod y 2 == 0], mod x 3 == 0] = [6,12,18]

--c
--[(x,y) | x <- [0..20], y <- [0..20], x+y == 30] = [(10,20),(11,19),(12,18),(13,17),(14,16),(15,15),(16,14),(17,13),(18,12),(19,11),(20,10)]

--d
--[sum [y | y <- [1..x], odd y] | x <- [1..10]] = [1,1,4,4,9,9,16,16,25,25]

--9
--a
--[2^x | x <- [0..10]]

--b
--[(x,y) | x <- [1..5], y <- [1..5], x+y == 6]

--c
--[[1..x] | x <- [1..5]]

--d
--[replicate x 1 | x <- [1..5]]

--e
--[fatorial x | x <- [1..6]]

fatorial :: Int -> Int
fatorial 0 = 1
fatorial x = x * fatorial (x-1)
