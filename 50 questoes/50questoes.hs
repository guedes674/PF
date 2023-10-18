--1
enumFromTo' :: Int -> Int -> [Int]
enumFromTo' x y | x < y = x : enumFromTo' (x+1) y
                | otherwise = [x]

--2
enumFromThenTo' :: Int -> Int -> Int -> [Int]
enumFromThenTo' x esp y | x < y && x+esp <= y = x : enumFromThenTo' ((x-1)+esp) esp y
                        | otherwise = [x]

--3
plusplus :: [a] -> [a] -> [a]
plusplus [] l = l
plusplus l [] = l
plusplus l1 l2  = l1 ++ plusplus l2 []

--4
excexc :: [a] -> Int -> a
excexc [] _ = error "lista vazia"
excexc (h:t) x | x /= 0 = excexc t (x-1)
               | otherwise = h

--5
reverse' :: [a] -> [a]
reverse' [] = [] 
reverse' l = last l : reverse' (init l)

--6
take' :: Int -> [a] -> [a]
take' _ [] = []
take' x (h:t) | x >= length (h:t) && x /= 0 = h:t
              | x /= 0 = h : take' (x-1) t
              | otherwise = []

--7
drop' :: Int -> [a] -> [a]
drop' x (h:t) | x >= length (h:t) && x /= 0 = h:t
              | x /= 0 = drop' (x-1) t
              | otherwise = h:t

--8
zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys

--9
replicate' :: Int -> a -> [a]
replicate' n x | n /= 0 = x : replicate' (n-1) x
               | otherwise = []

--10
intersperse' :: a -> [a] -> [a]
intersperse' x [y] = [y]
intersperse' x (h:t) = h : x : intersperse' x t

--11
group' :: Eq a => [a] -> [[a]]
group' [] = []
group' [a] = [[a]]
group' (a:b:t) | a == b = (a:b:takeWhile' a t) : group' (dropWhile' a t)
               | otherwise = [a] : group' (b:t)

--Funcoes Ajudantes
takeWhile' :: Eq a => a -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' x (h:t) | x == h = h : takeWhile' x t
                   | otherwise = takeWhile' x t

dropWhile' :: Eq a => a -> [a] -> [a]
dropWhile' _ [] = []
dropWhile' x (h:t) | x == h = dropWhile' x t
                   | otherwise = h : dropWhile' x t

--12
concat' :: [[a]] -> [a]
concat' [] = []
concat' (h:t) = h ++ concat' t

--13
inits' :: [a] -> [[a]]
inits' [a] = [[]] ++ [[a]]
inits' l = inits' (init l) ++ [l]

--14
tails' :: [a] -> [[a]]
tails' [a] = [[a]] ++ [[]]
tails' l = [l] ++ tails' (tail l)

--15
heads :: [[a]] -> [a]
heads [] = []
heads ([]:ts) = heads ts
heads ((h:t):ts) = h : heads ts

--16
total :: [[a]] -> Int
total [] = 0
total (h:t) = aux1 h + total t

aux1 :: [a] -> Int 
aux1 [] = 0
aux1 (h:t) = 1 + aux1 t

--17
fun :: [(a,b,c)] -> [(a,c)]
fun [] = []
fun ((x,y,z):t) = (x,z) : fun t

--18
cola :: [(String,b,c)] -> String
cola [] = []
cola ((s,x,y):t) = s ++ cola t

--19
idade :: Int -> Int -> [(String,Int)] -> [String]
idade _ _ [] = []
idade x y ((nome,nasc):t) | x - nasc >= y = nome : idade x y t
                          | otherwise = idade x y t

--20
powerEnumFrom :: Int -> Int -> [Int]
powerEnumFrom n m = aux2 n m 0

aux2 :: Int -> Int -> Int -> [Int]
aux2 n m acc | acc < m-1 = n^acc : aux2 n m (acc+1)
             | acc == m-1 = [n^acc]
             | otherwise = []

--21
isPrime :: Int -> Bool
isPrime x | x < 2 = error"número inválido"
          | otherwise = aux3 x 2

aux3 :: Int -> Int -> Bool
aux3 x acc | acc >= 2 && acc <= toInt(sqrt (fromIntegral x)) && mod x acc == 0 = False
           | otherwise = True

toInt :: Float -> Int
toInt x = round x

--22
isPrefixOf :: Eq a => [a] -> [a] -> Bool
isPrefixOf _ [] = False
isPrefixOf [] _ = True
isPrefixOf (x:xs) (y:ys) | x == y = isPrefixOf xs ys
                         | otherwise = False

--23
isSuffixOf :: Eq a => [a] -> [a] -> Bool
isSuffixOf _ [] = False
isSuffixOf [] _ = True
isSuffixOf l1 l2 | last l1 == last l2 = isSuffixOf (init l1) (init l2)
                 | otherwise = False

--24
isSubsequenceOf' :: Eq a => [a] -> [a] -> Bool
isSubsequenceOf' [] _ = True
isSubsequenceOf' _ [] = False
isSubsequenceOf' (x:xs) (y:ys) | x == y = isSubsequenceOf' xs ys
                               | otherwise = isSubsequenceOf' (x:xs) ys

--25
elemIndices' :: Eq a => a -> [a] -> [Int]
elemIndices' _ [] = []
elemIndices' x (h:t) = aux4 x (h:t) 0

aux4 :: Eq a => a -> [a] -> Int -> [Int]
aux4 x [] accpos = []
aux4 x (h:t) accpos | x == h = accpos : aux4 x t (accpos+1)
                    | otherwise = aux4 x t (accpos+1)

--26
nub' :: Eq a => [a] -> [a]
nub' [] = []
nub' (h:t) | aux5 h t = nub' t
           | otherwise = h : nub' t

aux5 :: Eq a => a -> [a] -> Bool
aux5 x [] = False
aux5 x (h:t) | x == h = True
             | otherwise = aux5 x t

--27
delete' :: Eq a => a -> [a] -> [a]
delete' x (h:t) | x == h = t
                | otherwise = h : delete' x t

--28
barras :: Eq a => [a] -> [a] -> [a]
barras [] _ = []
barras l [] = l
barras (x:xs) (y:ys) | y == x = barras xs ys
                     | otherwise = x : barras xs (y:ys)

--29
union' :: Eq a => [a] -> [a] -> [a]
union' [] l = []
union' l [] = l
union' (x:xs) (y:ys) | aux5 x (y:ys) = union' ys (x:xs)
                     | otherwise = y : union' (x:xs) ys

--[1,1,2,3,4] [1,5]
--union' [5] [1,1,2,3,4]
--1 : union' [5] [1,2,3,4] ...

--30
intersect' :: Eq a => [a] -> [a] -> [a]
intersect' [] _ = []
intersect' (x:xs) (y:ys) | aux5 x (y:ys) = x : intersect' xs (y:ys)
                         | otherwise = intersect' xs (y:ys)

--31
insert' :: Ord a => a -> [a] -> [a]
insert' x [] = [x]
insert' x (h:t) | x > h = h : insert' x t
                | otherwise =  x : (h:t)

--32
unwords' :: [String] -> String
unwords' [] = []
unwords' [x] = x
unwords' (h:t) = h ++ " " ++ unwords' t

--33
unlines' :: [String] -> String
unlines' [] = []
unlines' (h:t) = h ++ "\n" ++ unlines' t

--34
pMaior :: Ord a => [a] -> Int
pMaior (h:t) | aux6 (h:t) == h = 0
             | otherwise = 1 + pMaior t

aux6 :: Ord a => [a] -> a
aux6 [x] = x
aux6 (h:t) | head t >= h = aux6 t
           | otherwise = aux6 (h:tail t)

--35
lookup' :: Eq a => a -> [(a,b)] -> Maybe b
lookup' _ [] = Nothing
lookup' x ((a,b):t) | x == a = Just b
                    | otherwise = lookup' x t

--36
preCrescente :: Ord a => [a] -> [a]
preCrescente [] = []
preCrescente (a:b:t) = if a <= b then a : preCrescente (b:t)
                       else [a]

--37
iSort :: Ord a => [a] -> [a]
iSort [] = []
iSort (h:t) = insert' h (iSort t)

--38
menor :: String -> String -> Bool
menor [] _ = True
menor _ [] = False
menor (x:xs) (y:ys) | x < y = True
                    | x > y = False
                    | otherwise = menor xs ys

--39
elemMSet :: Eq a => a -> [(a,Int)] -> Bool
elemMSet x [] = False
elemMSet x ((a,b):t) | x == a = True
                     | otherwise = elemMSet x t

--40
converteMSet :: [(a,Int)] -> [a]
converteMSet [] = []
converteMSet ((a,b):t) | b /= 0 = a : converteMSet ((a,b-1):t)
                       | otherwise = converteMSet t

--41
insereMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
insereMSet _ [] = []
insereMSet x ((a,b):t) | x == a = (a,b+1):t
                       | otherwise = (a,b) : insereMSet x t

--42
removeMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
removeMSet _ [] = []
removeMSet x ((a,b):t) | x == a && b > 1 = (a,b-1):t
                       | x == a && b <= 1 = t
                       | otherwise = (a,b) : removeMSet x t

--43
constroiMSet :: Ord a => [a] -> [(a,Int)]
constroiMSet [] = []
constroiMSet (h:t) = (h,aux7 (h:t) h) : constroiMSet (dropWhile' h t)

aux7 :: Ord a => [a] -> a -> Int
aux7 [] _ = 0
aux7 (h:t) x | h == x = 1 + aux7 t x
             | otherwise = aux7 t x

--44
partitionEithers' :: [Either a b] -> ([a],[b])
partitionEithers' [] = ([], [])
partitionEithers' (h:t)  = case h of
                           Left a -> (a:x, y)
                           Right b -> (x, b:y)
    where (x,y) = partitionEithers' t

--45
catMaybes' :: [Maybe a] -> [a]
catMaybes' [] = []
catMaybes' (h:t) = case h of
                  Nothing -> catMaybes' t
                  Just a -> a:catMaybes' t

--46
data Movimento = Norte | Sul | Este | Oeste deriving Show

caminho :: (Int,Int) -> (Int,Int) -> [Movimento]
caminho (xi,yi) (xf,yf) | xi < xf = Este : caminho (xi+1,yi) (xf,yf)
                        | xi > xf = Oeste : caminho (xi-1,yi) (xf,yf)
                        | yi < yf && xi == xf = Norte : caminho (xi,yi+1) (xf,yf)
                        | yi > yf && xi == xf = Sul : caminho (xi,yi-1) (xf,yf)
                        | xi == xf && yi == yf = []

--47
hasLoops :: (Int, Int) -> [Movimento] -> Bool
hasLoops (x,y) [] = False
hasLoops (x,y) (h:t) = case h of
                        Norte -> aux8 (x,y) (x,y+1) t
                        Sul -> aux8 (x,y) (x,y-1) t
                        Este -> aux8 (x,y) (x+1,y) t
                        Oeste -> aux8 (x,y) (x-1,y) t

aux8 :: (Int, Int) -> (Int, Int) -> [Movimento] -> Bool
aux8 (xi, yi) (xa, ya) [] = if xi == xa && yi == ya then True
                             else False
aux8 (xi, yi) (xa,ya) (h:t) = if xi == xa && yi == ya then True
                              else case h of
                                    Norte -> aux8 (xi,yi) (xa,ya+1) t
                                    Sul -> aux8 (xi,yi) (xa,ya-1) t
                                    Oeste -> aux8 (xi,yi) (xa-1,ya) t
                                    Este -> aux8 (xi,yi) (xa+1, ya) t

--48
type Ponto = (Float,Float)
data Rectangulo = Rect Ponto Ponto

contaQuadrados :: [Rectangulo] -> Int
contaQuadrados [] = 0
contaQuadrados (h:t) | verificaQ h = 1 + contaQuadrados t
                     | otherwise = contaQuadrados t

verificaQ :: Rectangulo -> Bool
verificaQ (Rect (x1,y1) (x2,y2)) | (x2-x1) == (y2-y1) = True
                                 | otherwise = False

--49
areaTotal :: [Rectangulo] -> Float
areaTotal [] = 0
areaTotal ((Rect (x1,y1) (x2,y2)):t) = (x2-x1) * (y2-y1) + areaTotal t

--50
data Equipamento = Bom | Razoavel | Avariado deriving Show

naoReparar :: [Equipamento] -> Int
naoReparar [] = 0
naoReparar (h:t) = case h of
                   Bom -> 1 + naoReparar t
                   Razoavel -> 1 + naoReparar t
                   Avariado -> naoReparar t
