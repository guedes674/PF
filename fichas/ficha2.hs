import Data.Char
import GHC.Builtin.Names.TH (nominalRName)
--1
--a
funA :: [Double] -> Double
funA [] = 0
funA (y:ys) = y^2 + (funA ys)

--funA [2,3,5,1] = 39.0

--b
funB :: [Int] -> [Int]
funB [] = []
funB (h:t) = if (mod h 2)==0 then h : (funB t)
             else (funB t)

--funB [8,5,12] = [8,12]

--c
funC :: [a] -> [a]
funC (x:y:t) = funC t
funC [x] = [x]
funC [] = []

--funC [1,2,3,4,5] = [5]

--d
funD :: [a] -> [a]
funD l = g [] l

g :: [a] -> [a] -> [a]
g acc [] = acc
g acc (h:t) = g (h:acc) t

--funD "otrec" = "certo"

--funD "otrec" = g [] "otrec"
--             = g (o:[]) "trec"
--             = g (t:o:[]) "rec"
--             ...

--2
--a
dobros :: [Float] -> [Float]
dobros [] = []
dobros (h:t) = (2*h) : dobros t

--b
numOcorre :: Char -> String -> Int
numOcorre _ [] = 0
numOcorre c (h:t) | c == h = 1 + numOcorre c t
                  | otherwise = numOcorre c t

--c
positivos :: [Int] -> Bool
positivos [] = True
positivos (h:t) | h >= 0 = positivos t
                | otherwise = False

--d
soPos :: [Int] -> [Int]
soPos [] = []
soPos (h:t) | h >= 0 = h : soPos t
            | otherwise = soPos t

--e
somaNeg :: [Int] -> Int
somaNeg [] = 0
somaNeg (h:t) | h < 0 = h + somaNeg t
              | otherwise = somaNeg t

--f
tresUlt :: [a] -> [a]
tresUlt l | length l < 3 = l
          | length l == 3 = l
          | otherwise = tresUlt (tail l)

--g
segundos :: [(a,b)] -> [b]
segundos [] = []
segundos ((a,b):t) = b : segundos t

--h
nosPrimeiros :: (Eq a) => a -> [(a,b)] -> Bool
nosPrimeiros _ [] = False
nosPrimeiros x ((a,b):t) | x == a = True
                         | otherwise = nosPrimeiros x t

--i
sumTriplos :: (Num a, Num b, Num c) => [(a,b,c)] -> (a,b,c)
sumTriplos [(a,b,c)] = (a,b,c)
sumTriplos ((a,b,c):(d,e,f):t) = sumTriplos ((a+d,b+e,c+f):t)

--3
--a
soDigitos :: [Char] -> [Char]
soDigitos [] = []
soDigitos (h:t) | ord h >= ord '0' && ord h <= ord '9' = h : soDigitos t
                | otherwise = soDigitos t

--b
minusculas :: [Char] -> Int
minusculas [] = 0
minusculas (h:t) | ord h >= ord 'a' && ord h <= ord 'z' = 1 + minusculas t
                 | otherwise = minusculas t

--c
nums :: String -> [Int]
nums [] = []
nums (h:t) | ord h >= ord '0' && ord h <= ord '9' = digitToInt h : nums t
           | otherwise = nums t

--4
type Polinomio = [Monomio]
type Monomio = (Float,Int)
--a
conta :: Int -> Polinomio -> Int
conta _ [] = 0
conta n ((x,g):t) | n == g = 1 + conta n t
                  | otherwise = conta n t

--b
grau :: Polinomio -> Int
grau ((x,g):t) = aux1 ((x,g):t) 0

aux1 :: Polinomio -> Int -> Int
aux1 [] gMax = gMax
aux1 ((x,g):t) gMax | g >= gMax = aux1 t g
                    | otherwise = aux1 t gMax

--c
selgrau :: Int -> Polinomio -> Polinomio
selgrau _ [] = []
selgrau gSel ((x,g):t) | gSel == g = (x,g) : selgrau gSel t
                       | otherwise = selgrau gSel t

--d
deriv :: Polinomio -> Polinomio
deriv [] = []
deriv ((x,g):t) = (fromIntegral g * x,g-1) : deriv t

--e
calcula :: Float -> Polinomio -> Float
calcula _ [] = 0
calcula v ((x,g):t) = x * v^g + calcula v t

--f
simp :: Polinomio -> Polinomio
simp [] = []
simp ((x,g):t) | g == 0 = simp t
               | otherwise = (x,g) : simp t

--g
mult :: Monomio -> Polinomio -> Polinomio
mult _ [] = []
mult (x1,g1) ((x2,g2):t) = (x1*x2,g1+g2) : mult (x1,g1) t

--h
normaliza :: Polinomio -> Polinomio
normaliza [] = []
normaliza ((x,g):t) = aux2 (normaliza t) (x,g)

aux2 :: Polinomio -> Monomio -> Polinomio
aux2 [] (x,g) = [(x,g)]
aux2 ((x,g):t) (x1,g1) | g == g1 = aux2 t (x1+x,g1)
                       | otherwise = (x,g) : aux2 t (x1,g1)

--i
soma :: Polinomio -> Polinomio -> Polinomio
soma [] p = p
soma p [] = p
soma ((x1,g1):t1) ((x2,g2):t2) | g1 == g2 = normaliza (soma ((x1+x2,g1):t1) t2)
                               | otherwise = normaliza ((x1,g1):(x2,g2):soma t1 t2)

--j
produto :: Polinomio -> Polinomio -> Polinomio
produto [] p = p
produto p [] = p
produto ((x1,g1):t1) p2 = soma (mult (x1,g1) p2) (produto t1 p2)

--k
ordena :: Polinomio -> Polinomio
ordena [] = []
ordena ((x,g):t) = aux3 (x,g) (ordena t)

aux3 :: Monomio -> Polinomio -> Polinomio
aux3 (x,g) [] = [(x,g)]
aux3 (x1,g1) ((x2,g2):t) | g1 > g2 = (x2,g2) : aux3 (x1,g1) t
                         | otherwise = ((x1,g1):(x2,g2):t)

--l
equiv :: Polinomio -> Polinomio -> Bool
equiv p1 p2 | ordena (normaliza p1) == ordena (normaliza p2) = True
            | otherwise = False
