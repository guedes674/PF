import Data.List
import Data.Char
--1
data Frac = F Integer Integer
--a
normaliza :: Frac -> Frac
normaliza (F a b) = F (div a m) (div b m)
    where m = mdc a b

mdc :: Integer -> Integer -> Integer
mdc _ a = a
mdc a _ = a
mdc a b = mod (a+b) b

--b
instance Eq Frac where
    (F x y) == (F a b) = (x*b) == (y*a)

--c
instance Ord Frac where
    compare (F x1 y1) (F x2 y2) | c1 > c2 = GT
                                | c1 == c2 = EQ
                                | c1 < c2 = LT
        where c1 = (fromIntegral x1)/(fromIntegral y1)
              c2 = (fromIntegral x2)/(fromIntegral y2)

--d
instance Show Frac where
    show (F x y) = show x ++ "/" ++ show y

--e
instance Num Frac where
    (F x1 y1) + (F x2 y2) = normaliza (F ((x1*y2)+(x2*y1)) (y1*y2))
    (F x1 y1) * (F x2 y2) = normaliza (F (x1*x2) (y1*y2))
    (F x1 y1) - (F x2 y2) = normaliza (F ((x1*y2)-(x2*y1)) (y1*y2))
    negate (F x y) = F ((-1)*x) y
    abs (F x y) = F (abs x) (abs y)
    signum (F x y) | (x > 0 && y > 0) || (x < 0 && y < 0) = 1
                   | x == 0 = 0
                   | otherwise = -1
    fromInteger x = (F x 1)

--f
dobroDeF :: Frac -> [Frac] -> [Frac]
dobroDeF f l = filter (\(F x y) -> (F x y) > f2) l
    where f2 = f * (F 2 1)

--2
data Exp a = Const a
    | Simetrico (Exp a)
    | Mais (Exp a) (Exp a)
    | Menos (Exp a) (Exp a)
    | Mult (Exp a) (Exp a)

--a
instance Show a => Show (Exp a) where
    show (Const a) = show a
    show (Simetrico a) = "(-" ++ show a ++ ")"
    show (Mais a b) = "(" ++ show a ++ "+" ++ show b ++ ")"
    show (Menos a b) = "(" ++ show a ++ "-" ++ show b ++ ")"
    show (Mult a b) = "(" ++ show a ++ "*" ++ show b ++ ")"

valor :: (Num a) => Exp a -> a
valor (Const a) = a
valor (Simetrico a) = - (valor a)
valor (Mais a b) = (valor a) + (valor b)
valor (Menos a b) = (valor a) - (valor b)
valor (Mult a b) = (valor a) * (valor b)

--b
instance (Num a, Eq a) => Eq (Exp a) where
    x == y = (valor x) == (valor y)

--c
instance (Num a, Eq a) => Num (Exp a) where
    x + y = Const ((valor x) + (valor y))
    x * y = Const ((valor x) * (valor y))
    x - y = Const ((valor x) - (valor y))
    negate (Const x) = Const (- x)
    negate (Simetrico x) = x
    negate (Mais x y) = Mais (-x) (-y)
    negate (Menos x y) = Menos (-x) (-y)
    negate (Mult x y) = Mult (-x) y
    abs (Const x) = Const (abs x)
    abs (Simetrico x) = abs x
    abs (Mais x y) = Mais (abs x) (abs y)
    abs (Menos x y) = Menos (abs x) (abs y)
    abs (Mult x y) = Mult (abs x) (abs y)
    signum (Const a) = if (abs a) == a then if a == 0 then 0 
                                            else 1 
                       else -1
    signum (Simetrico a) = - signum a
    signum (Mais a b) = if abs (a + b) == a + b then if a + b == 0 then 0
                                            else 1 
                       else -1
    signum (Menos a b) = if abs (a - b) == a - b then if a - b == 0 then 0
                                            else 1 
                       else -1
    signum (Mult a b) = if abs (a * b) == a * b then if a * b == 0 then 0
                                            else 1 
                       else -1

--3
data Movimento = Credito Float | Debito Float
data Data = D Int Int Int
data Extracto = Ext Float [(Data, String, Movimento)]
--a
--instance Ord Data where 
--    compare (D dia1 mes1 ano1) (D dia2 mes2 ano2) | ano1 > ano2 || ano1 == ano2 && mes1 > mes2 || ano1 == ano2 && mes1 == mes2 && dia1 > dia2 = GT
--                                                  | ano1 == ano2 && mes1 == mes2 && dia1 == dia2 = EQ
--                                                  | otherwise = LT

--b
instance Show Data where
    show (D dia mes ano) = show dia ++ "/" ++ show mes ++ "/" ++ show ano

--c
--ordena :: Extracto -> Extracto
--ordena (Ext a l) = Ext a (sortBy (\(dat1,_,_) (dat2,_,_) -> compare dat1 dat2) l)

--d
instance Show Extracto where
    show (Ext n l) = "Saldo anterior: " ++ show n ++
                     "\n---------------------------------------" ++
                     "\nData       Descricao   Credito   Debito" ++
                     "\n---------------------------------------\n" ++ concatMap (\(dat,str,_) -> show dat ++ replicate (11 - length (show dat)) ' ' ++ map toUpper str ++ "    \n") l ++
                     "---------------------------------------" ++
                     "\nSaldo actual: " ++ show (saldo (Ext n l))

saldo :: Extracto -> Float
saldo (Ext s l) = foldl (\acc (dat,s,mov) -> case mov of Credito a -> acc + a
                                                         Debito a -> acc - a) s l
