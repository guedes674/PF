import Data.List

--1
--a
any' :: (a -> Bool) -> [a] -> Bool
any' f [] = False
any' f (h:t) | f h = True
             | otherwise = any' f t

--b
zipWith' :: (a->b->c) -> [a] -> [b] -> [c]
zipWith' _ _ [] = []
zipWith' _ [] _ = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

--c
takeWhile' :: (a->Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' f (h:t) | f h = h : takeWhile' f t
                   | otherwise = takeWhile' f t

--d
dropWhile' :: (a->Bool) -> [a] -> [a]
dropWhile' _ [] = []
dropWhile' f (h:t) | f h = dropWhile' f t
                   | otherwise = h : dropWhile' f t

--e
span' :: (a-> Bool) -> [a] -> ([a],[a])
span' _ [] = ([],[])
span' f (h:t) | f h = (h:x,y)
              | otherwise = ([],h:t)
    where (x,y) = span' f t

--f
deleteBy' :: (a -> a -> Bool) -> a -> [a] -> [a]
deleteBy' _ _ [] = []
deleteBy' f x (h:t) | f x h = deleteBy' f x t
                    | otherwise = h : deleteBy' f x t

--g
sortOn' :: Ord b => (a -> b) -> [a] -> [a]
sortOn' _ [] = []
sortOn' f (h:t) = aux1 f h (sortOn' f t)

aux1 :: Ord b => (a -> b) -> a -> [a] -> [a]
aux1 f x [] = [x]
aux1 f x (h:t) | f x > f h = h : aux1 f x t
               | otherwise = x : h : t

--2
type Polinomio = [Monomio]
type Monomio = (Float,Int)
--a
selgrau :: Int -> Polinomio -> Polinomio
selgrau a p = filter (\(x,g) -> g == a) p

--b
conta :: Int -> Polinomio -> Int
conta a p = length (filter (\(x,g) -> g == a) p)
--ou
contaf :: Int -> Polinomio -> Int
contaf n p = foldl (\acc x -> if n == snd x then acc+1 else acc) 0 p

--c
grau :: Polinomio -> Int
grau p = maximum (map snd p)
--ou
grauf :: Polinomio -> Int
grauf p = foldl (\acc x -> if snd x > acc then snd x else acc) 0 p

--d
deriv :: Polinomio -> Polinomio
deriv p = filter (\(x,g) -> g >= 0) (map (\(x,g) -> (x * fromIntegral g,g-1)) p)

--e
calcula :: Float -> Polinomio -> Float
calcula a p = sum (map (\(x,g) -> a*x^g) p)
--ou
calculaf :: Float -> Polinomio -> Float
calculaf v p = foldl (\acc x -> acc + (fst x) * v ^ snd x) 0 p

--f
simp :: Polinomio -> Polinomio
simp p = filter (\(x,g) -> g>0) p

--g
mult :: Monomio -> Polinomio -> Polinomio
mult (x,g) p = map (\(x1,g1) -> (x1*x,g1+g)) p
--ou
multf :: Monomio -> Polinomio -> Polinomio
multf (x,g) p = foldl (\acc (x2,g2) -> acc ++ [(x2*x,g2+g)]) [] p

--h
ordena :: Polinomio -> Polinomio
ordena ((x,g):t) = sortOn snd ((x,g):t)

--i
normaliza :: Polinomio -> Polinomio
normaliza (x:xs) = foldl (\((n1,g1):t) (n2,g2) -> if g1 == g2 then (n1+n2, g1):t else (n1,g1):(n2,g2):t) [x] (ordena xs)

--j
soma :: Polinomio -> Polinomio -> Polinomio
soma p1 p2 = normaliza (p1 ++ p2)

--k
produto :: Polinomio -> Polinomio -> Polinomio
produto p1 p2 = foldl (\acc x -> soma (mult x p2) acc) [] p1

--l
equiv :: Polinomio -> Polinomio -> Bool
equiv p1 p2 | normaliza p1 == normaliza p2 = True
            | otherwise = False

--3
type Mat a = [[a]]
--a
dimOK :: Mat a -> Bool
dimOK (x:xs) = all (\y -> length y == length x) xs

--b
dimMat :: Mat a -> (Int,Int)
dimMat [] = (0,0)
dimMat (x:xs) = if dimOK (x:xs) then (length x,length (x:xs))
                else (-1,-1)

--c
addMat :: Num a => Mat a -> Mat a -> Mat a
addMat m [] = m
addMat [] m = m
addMat (x:xs) (y:ys) = if dimOK (x:xs) && dimOK (y:ys) then zipWith (+) x y : addMat xs ys
                       else [[]]

--d
transpose' :: Mat a -> Mat a
transpose' ([]:_) = []
transpose' m = map head m : transpose' (map tail m)

--e
multMat :: Num a => Mat a -> Mat a -> Mat a
multMat _ ([]:_) = []
multMat ([]:_) _ = []
multMat m1 m2 = [[sum(zipWith (*) l1 l2) | l1 <- m1]| l2 <- transpose m2]

--f
zipWMat :: (a -> b -> c) -> Mat a -> Mat b -> Mat c
zipWMat _ [] _ = []
zipWMat _ _ [] = []
zipWMat f (x:xs) (y:ys) = zipWith f x y : zipWMat f xs ys

--g (Adaptação na definição pois não consegui encontrar de outra forma)
triSup :: (Eq a, Num a) => Mat a -> Bool
triSup [] = True
triSup (x:xs) = all (==0) h && triSup t
    where h = map head xs
          t = map tail xs

--h
rotateLeft :: Mat a -> Mat a
rotateLeft m = transpose' (map reverse m)
