--1
data Hora2 = H Int Int deriving (Show,Eq)
type Etapa = (Hora2,Hora2)
type Viagem = [Etapa]
--a
testEtapa :: Etapa -> Bool
testEtapa (H hi mi,H hf mf) | hi < hf = True
                            | hi == hf && mi <= mf = True
                            | otherwise = False

--b
testViagem :: Viagem -> Bool
testViagem [(H hi mi,H hf mf)] = True
testViagem ((H hi1 mi1,H hf1 mf1):(H hi2 mi2,H hf2 mf2):t) | testEtapa (H hi1 mi1,H hf1 mf1) && testEtapa (H hi2 mi2,H hf2 mf2) && hf2 > hf1 = testViagem ((H hi2 mi2,H hf2 mf2):t)
                                                           | testEtapa (H hi1 mi1,H hf1 mf1) && testEtapa (H hi2 mi2,H hf2 mf2) && hf2 == hf1 && mf2 > mf1 = testViagem ((H hi2 mi2,H hf2 mf2):t)
                                                           | otherwise = False

--c
partCheg :: Viagem -> (Hora2,Hora2)
partCheg ((H hi mi,H hf mf):t) | testViagem ((H hi mi,H hf mf):t) = (H hi mi, snd (last t))
                               | otherwise = error "a viagem e invalida"

--d
viagemEfetiva :: Viagem -> Hora2
viagemEfetiva [] = H 0 0
viagemEfetiva ((h1,h2):t) = addMin' (viagemEfetiva t) (convertHr' (difHr' (h1,h2)))

--e
espera :: Viagem -> Hora2
espera v = difHr' (viagemTotal v, viagemEfetiva v)

--f
viagemTotal :: Viagem -> Hora2
viagemTotal v = difHr' (partCheg v)

--Funcoes da ficha 1
convertMin' :: Int -> Hora2
convertMin' m = H horas minutos
    where horas = div m 60
          minutos = mod m 60

convertHr' :: Hora2 -> Int
convertHr' (H h m) = (h*60) + m

difHr' :: (Hora2,Hora2) -> Hora2
difHr' (H h1 m1,H h2 m2) = convertMin' (abs (h2-h1)*60 + abs (m2-m1))

addMin' :: Hora2 -> Int -> Hora2
addMin' (H h m) x | m + x >= 60 = H (h+1) (abs (60 - (m + x)))
                  | otherwise = H h (m+x)

--2
type Poligonal = [Ponto]
data Ponto = Cartesiano Double Double | Polar Double Double deriving (Show,Eq)
data Figura = Circulo Ponto Double | Rectangulo Ponto Ponto | Triangulo Ponto Ponto Ponto deriving (Show,Eq)
--a
distLinha :: Poligonal -> Double
distLinha [a] = 0
distLinha (a:b:t) = distp a b + distLinha (b:t)

--b
fechada :: Poligonal -> Bool
fechada (a:t) = aux1 t a

aux1 :: Poligonal -> Ponto -> Bool
aux1 [] _ = False
aux1 (a:t) x | posx x == posx a && posy x == posy a = True
             | otherwise = aux1 t x

--c
triangula :: Poligonal -> [Figura]
triangula [a,b,c] = [Triangulo a b c]
triangula (a:b:c:t) = Triangulo a b c : triangula (a:c:t)

--d
areaLinha :: Poligonal -> Double
areaLinha p = aux2 (triangula p)

aux2 :: [Figura] -> Double
aux2 [] = 0
aux2 (t1:rest) = area t1 + aux2 rest

--e
mover :: Poligonal -> Ponto -> Poligonal
mover (a:b:t) x = x:b:t

--f
zoom :: Double -> Poligonal -> Poligonal
zoom e [a,Cartesiano x y] = [a, Cartesiano (x*e) (y*e)]
zoom e (a:(Cartesiano x y):t) = a : zoom e (Cartesiano (x*e) (y*e):t)

--Funcoes da ficha 1
distp :: Ponto -> Ponto -> Double
distp p1 p2 = sqrt((posx p1 - posx p2)^2 + (posy p1 - posy p2)^2)

posx :: Ponto -> Double
posx (Cartesiano x y) = abs x
posx (Polar r ang) = if ang == pi/2 then 0 else r * cos ang

posy :: Ponto -> Double
posy (Cartesiano x y) = abs y
posy (Polar r ang) = if ang == pi then 0 else r * sin ang

area :: Figura -> Double
area (Triangulo p1 p2 p3) =
    let a = distp p1 p2
        b = distp p2 p3
        c = distp p3 p1
        s = (a+b+c) / 2 -- semi-perimetro
    in sqrt (s*(s-a)*(s-b)*(s-c)) -- fórmula de Heron
area (Circulo _ r) = pi * (r ^ 2)
area (Rectangulo p1 p2) = abs (posx p2 - posx p1) * abs (posy p2 - posy p1)

--3
data Contacto = Casa Integer | Trab Integer | Tlm Integer | Email String deriving Show
type Nome = String
type Agenda = [(Nome, [Contacto])]
--a
acrescEmail :: Nome -> String -> Agenda -> Agenda
acrescEmail _ _ [] = []
acrescEmail n e ((nome,c:t1):t2) | nome == n = (nome,(c:t1) ++ [Email e]) : t2
                                 | otherwise = (nome,c:t1) : acrescEmail n e t2

--b
verEmails :: Nome -> Agenda -> Maybe [String]
verEmails _ [] = Nothing
verEmails n ((nome,c):t) | n == nome && justEmails c == [] = Nothing
                         | n == nome = Just (justEmails c)
                         | otherwise = verEmails n t

justEmails ::  [Contacto] -> [String]
justEmails [] = []
justEmails (c:t) = case c of
                    Email x -> x : justEmails t
                    _ -> justEmails t

--c
consTelefs :: [Contacto] -> [Integer]
consTelefs [] = []
consTelefs (c:t) = case c of
                    Tlm x -> x : consTelefs t
                    Casa x -> x : consTelefs t
                    Trab x -> x : consTelefs t
                    _ -> consTelefs t

--d
casa :: Nome -> Agenda -> Maybe Integer
casa _ [] = Nothing
casa n ((nome,c):t) | n == nome = Just (conCasa c)
                    | otherwise = casa n t

conCasa :: [Contacto] -> Integer
conCasa [] = error "nao tem telefone de casa"
conCasa (c:t) = case c of
                    Casa x -> x
                    _ -> conCasa t

--4
type Dia = Int
type Mes = Int
type Ano = Int
type Nome2 = String
data Data = D Dia Mes Ano deriving Show
type TabDN = [(Nome,Data)]
--a
procura :: Nome2 -> TabDN -> Maybe Data
procura _ [] = Nothing
procura nome ((n,d):t) | nome == n = Just d
                       | otherwise = procura nome t

--b
idade :: Data -> Nome -> TabDN -> Maybe Int
idade _ _ [] = Nothing
idade (D d1 m1 a1) nome ((n,D d2 m2 a2):t) | nome == n = Just (idadeNome (D d2 m2 a2) (D d1 m1 a1))
                                           | otherwise = idade (D d1 m1 a1) nome t

idadeNome :: Data -> Data -> Int
idadeNome (D d1 m1 a1) (D d2 m2 a2) | m1 == m2 && d1 > d2 = (a2 - a1) - 1
                                    | m1 > m2 = (a2 - a1) - 1
                                    | otherwise = a2 - a1

--c
anterior :: Data -> Data -> Bool
anterior (D d1 m1 a1) (D d2 m2 a2) | a1 < a2 = True
                                   | a1 == a2 && m1 < m2 = True
                                   | a1 == a1 && m1 == m2 && d1 < d2 = True
                                   | otherwise = False

--d
ordena :: TabDN -> TabDN
ordena [] = []
ordena ((n,d):t) = insereNaTabela n d (ordena t)

insereNaTabela :: Nome -> Data -> TabDN -> TabDN
insereNaTabela nome d1 [] = [(nome,d1)]
insereNaTabela nome d1 ((n,d2):t) | anterior d1 d2 = (nome,d1) : (n,d2) : t
                                  | otherwise = (n,d2) : insereNaTabela nome d1 t

--e
porIdade:: Data -> TabDN -> [(Nome,Int)]
porIdade _ [] = []
porIdade d1 ((n,d2):t) = ordenaPorIdadesL ((n,idadeAdapt d1 n ((n,d2):t)) : porIdade d1 t)

idadeAdapt :: Data -> Nome -> TabDN -> Int
idadeAdapt _ _ [] = 0
idadeAdapt (D d1 m1 a1) nome ((n,D d2 m2 a2):t) | nome == n = idadeNome (D d2 m2 a2) (D d1 m1 a1)
                                                | otherwise = idadeAdapt (D d1 m1 a1) nome t

ordenaPorIdadesL :: [(Nome,Int)] -> [(Nome,Int)]
ordenaPorIdadesL [] = []
ordenaPorIdadesL ((n,i):t) = ordenaPorIdadesLAux n i (ordenaPorIdadesL t)

ordenaPorIdadesLAux :: Nome -> Int -> [(Nome,Int)] -> [(Nome,Int)]
ordenaPorIdadesLAux n i [] = [(n,i)]
ordenaPorIdadesLAux nome idade ((n,i):t) | idade < i = (nome,idade) : (n,i) : t
                                        | otherwise = (n,i) : ordenaPorIdadesLAux nome idade t

--5
data Movimento = Credito Float | Debito Float deriving Show
data Data2 = D2 Int Int Int deriving Show
data Extracto = Ext Float [(Data2, String, Movimento)] deriving Show
--a
extValor :: Extracto -> Float -> [Movimento]
extValor (Ext _ []) _ = []
extValor (Ext x ((_,_,m):t)) q = case m of
                                 Credito y -> if y > q then m : extValor (Ext x t) q
                                              else extValor (Ext x t) q
                                 Debito y -> if y > q then m : extValor (Ext x t) q
                                             else extValor (Ext x t) q

--b
filtro :: Extracto -> [String] -> [(Data2,Movimento)]
filtro (Ext _ []) _ = []
filtro (Ext x ((dt,desc,m):t)) descs | aux3 (dt,desc,m) descs = (dt,m) : filtro (Ext x t) descs
                                      | otherwise = filtro (Ext x t) descs

aux3 :: (Data2, String, Movimento) -> [String] -> Bool
aux3 _ [] = False
aux3 (dt,desc,m) (d:ds) | desc == d = True
                        | otherwise = aux3 (dt,desc,m) ds

--c
creDeb :: Extracto -> (Float,Float)
creDeb (Ext _ []) = (0,0)
creDeb (Ext x ((_,_,m):t)) = case m of
                             Credito y -> (y + a, b)
                             Debito y -> (a, y + b)
    where (a,b) = creDeb (Ext x t)

--d
saldo :: Extracto -> Float
saldo (Ext x []) = x
saldo (Ext x ((_,_,m):t)) = case m of
                            Credito y -> saldo (Ext (x+y) t)
                            Debito y -> saldo (Ext (x-y) t)
