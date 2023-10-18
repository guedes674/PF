import Data.Char
--1
--a
perimetro :: Double -> Double
perimetro raio = pi * raio

--b
dist :: (Double, Double) -> (Double, Double) -> Double
dist (x1,y1) (x2,y2) = sqrt(((x2-x1)^2)+((y2-y1)^2))

--c
primUlt :: [Int] -> (Int,Int)
primUlt (h:t) = (h,last t)

--d
multiplo :: Int -> Int -> Bool
multiplo x y | mod x y == 0 = True
             | otherwise = False

--e
truncaImpar :: [Int] -> [Int]
truncaImpar (h:t) | odd h = t
                  | otherwise = h:t

--f
max2 :: Int -> Int -> Int
max2 x y | x > y = x
         | otherwise = y

--g
max3 :: Int -> Int -> Int -> Int
max3 x y z = max2 (max2 x y) z

--2
--a
nRaizes :: Float -> Float-> Float -> Int
nRaizes a b c | binomio == 0 = 1
              | binomio > 0 = 2
              | otherwise = 0
    where binomio = sqrt(b^2-4*a*c)

--b
raizes :: Float -> Float -> Float -> [Float]
raizes a b c | nRaizes a b c == 0 = []
             | nRaizes a b c == 1 = [-b/2*a]
             | otherwise = [(-b + binomio)/2*a, (-b - binomio)/2*a]
    where binomio = sqrt(b^2-4*a*c)

--3
--type Hora = (Int,Int)
--a
testeHr :: (Int,Int) -> Bool
testeHr (h,m) | h >= 0 && h <= 23 && m >= 0 && m <= 59 = True
              | otherwise = False

--b
compHr :: (Int, Int) -> (Int, Int) -> Bool
compHr (h1,m1) (h2,m2) | h1 > h2 = True
                       | h1 == h2 && m1 >= m2 = True
                       | otherwise = False

--c
convertHr :: (Int,Int) -> Int
convertHr (h,m) = (h*60) + m

--d
convertMin :: Int -> (Int,Int)
convertMin m = (horas,minutos)
    where horas = div m 60
          minutos = mod m 60

--e
difHr :: (Int,Int) -> (Int,Int) -> Int
difHr (h1,m1) (h2,m2) = abs (h2-h1)*60 + abs (m2-m1)

--f
addMin :: (Int,Int) -> Int -> (Int,Int)
addMin (h,m) x | m + x >= 60 = (h+1,abs (60 - (m + x)))
               | otherwise = (h,m+x)

--4
data Hora = H Int Int deriving (Show,Eq)
--a
testeHr' :: Hora -> Bool
testeHr' (H h m) | h >= 0 && h < 24 && m >= 0 && m < 60 = True
                 | otherwise = False

--b
compHr' :: Hora -> Hora -> Bool
compHr' (H h1 m1) (H h2 m2) | h1 > h2 = True
                            | h1 == h2 && m1 >= m2 = True
                            | otherwise = False

--c
convertHr' :: Hora -> Int
convertHr' (H h m) = (h*60) + m

--d
convertMin' :: Int -> Hora
convertMin' m = H horas minutos
    where horas = div m 60
          minutos = mod m 60

--e
difHr' :: Hora -> Hora -> Int
difHr' (H h1 m1) (H h2 m2) = abs (h2-h1)*60 + abs (m2-m1)

--f
addMin' :: Hora -> Int -> Hora
addMin' (H h m) x | m + x >= 60 = H (h+1) (abs (60 - (m + x)))
                  | otherwise = H h (m+x)

--5
data Semaforo = Verde | Amarelo | Vermelho deriving (Show,Eq)
--a
next :: Semaforo -> Semaforo
next s | s == Verde = Amarelo
       | s == Amarelo = Vermelho
       | otherwise = Verde

--b
stop :: Semaforo -> Bool
stop s | s == Verde = False
       | otherwise = True

--c
safe :: Semaforo -> Semaforo -> Bool
safe s1 s2 | s1 == Vermelho || s2 == Vermelho = True
           | otherwise = False

--6
data Ponto = Cartesiano Double Double | Polar Double Double deriving (Show,Eq)
--a
posx :: Ponto -> Double
posx (Cartesiano x y) = abs x
posx (Polar r ang) = if ang == pi/2 then 0 else r * cos ang

--b
posy :: Ponto -> Double
posy (Cartesiano x y) = abs y
posy (Polar r ang) = if ang == pi then 0 else r * sin ang

--c
raio :: Ponto -> Double
raio (Cartesiano x y) = sqrt(x^2 + y^2)
raio (Polar r ang) = r

--d
angulo :: Ponto -> Double
angulo (Cartesiano x y) | x < 0 && y == 0 = pi
                        | x < 0 = pi + atan (y/x)
                        | otherwise = atan (y/x)
angulo (Polar r ang) = ang

--e
distp :: Ponto -> Ponto -> Double
distp p1 p2 = sqrt((posx p1 - posx p2)^2 + (posy p1 - posy p2)^2)

--7
data Figura = Circulo Ponto Double | Rectangulo Ponto Ponto | Triangulo Ponto Ponto Ponto deriving (Show,Eq)
--a
poligono :: Figura -> Bool
poligono f = case f of
             Circulo _ _ -> False
             Rectangulo _ _ -> True
             Triangulo _ _ _ -> True

--b
vertices :: Figura -> [Ponto]
vertices (Circulo _ _) = []
vertices (Rectangulo p1 p2) = [p1,Cartesiano (posx p2) (posy p1),p2,Cartesiano (posx p1) (posy p2)]
vertices (Triangulo p1 p2 p3) = [p1,p2,p3]

--c
area :: Figura -> Double
area (Triangulo p1 p2 p3) =
    let a = distp p1 p2
        b = distp p2 p3
        c = distp p3 p1
        s = (a+b+c) / 2 -- semi-perimetro
    in sqrt (s*(s-a)*(s-b)*(s-c)) -- fórmula de Heron
area (Circulo _ r) = pi * (r ^ 2)
area (Rectangulo p1 p2) = abs (posx p2 - posx p1) * abs (posy p2 - posy p1)

--d
perimetrof :: Figura -> Double
perimetrof (Triangulo p1 p2 p3) = distp p1 p2 + distp p2 p3 + distp p3 p1
perimetrof (Rectangulo p1 p2) = (distp p1 (Cartesiano (posx p2) (posy p1)))*2 + (distp (Cartesiano (posx p1) (posy p2)) p2)*2
perimetrof (Circulo p1 r) = pi*r

--8
--a
isLower' :: Char -> Bool
isLower' c | ord c >= ord 'a' && ord c <= ord 'z'= True
           | otherwise = False

--b
isDigit' :: Char -> Bool
isDigit' c | ord c >= ord '0' && ord c <= ord '9' = True
           | otherwise = False

--c
isAlpha' :: Char -> Bool
isAlpha' c | ord c >= ord 'A' && ord c <= ord 'z' = True
           | otherwise = False

--d
toUpper' :: Char -> Char
toUpper' c = chr (ord c - 32)

--e
intToDigit' :: Int -> Char
intToDigit' x = chr (x + 48)

--f
digitToInt' :: Char -> Int
digitToInt' c = ord c - 48
