--1
data ExpInt = Const Int
            | Simetrico ExpInt
            | Mais ExpInt ExpInt
            | Menos ExpInt ExpInt
            | Mult ExpInt ExpInt
--a
calcula :: ExpInt -> Int
calcula e = case e of
            Const a -> a
            Simetrico a -> - (calcula a)
            Mais a b -> (calcula a) + (calcula b)
            Menos a b -> (calcula a) - (calcula b)
            Mult a b -> (calcula a) * (calcula b)

--b
infixa :: ExpInt -> String
infixa e = case e of
           Const a -> show a
           Simetrico a -> "-" ++ infixa a
           Mais a b -> "(" ++ infixa a ++ " + " ++ infixa b ++ ")"
           Menos a b -> "(" ++ infixa a ++ " - " ++ infixa b ++ ")"
           Mult a b -> "(" ++ infixa a ++ " * " ++ infixa b ++ ")"

--c
posfixa :: ExpInt -> String
posfixa e = case e of
            Const a -> show a
            Simetrico a -> posfixa a ++ " -"
            Mais a b -> posfixa a ++ " " ++ posfixa b ++ " +"
            Menos a b -> posfixa a ++ " " ++ posfixa b ++ " -"
            Mult a b -> posfixa a ++ " " ++ posfixa b ++ " *"

--2
data RTree a = R a [RTree a] deriving Show

--a
soma :: Num a => RTree a -> a
soma (R r []) = r
soma (R r l) = r + sum (map soma l)

--b
altura :: RTree a -> Int
altura (R r []) = 1
altura (R r l) = 1 + maximum (map altura l)

--c
prune :: Int -> RTree a -> RTree a
prune x (R r l) | x == 0 = R r []
                | otherwise = R r (map (prune (x-1)) l)

--d
mirror :: RTree a -> RTree a
mirror (R r l) = R r (map mirror (reverse l))

--e
postorder :: RTree a -> [a]
postorder (R r l) = concat (map postorder l) ++ [r]

--3
data LTree a = Tip a | Fork (LTree a) (LTree a)

--a
ltSum :: Num a => LTree a -> a
ltSum (Tip a) = a
ltSum (Fork esq dir) = ltSum esq + ltSum dir

--b
listaLT :: LTree a -> [a]
listaLT (Tip a) = [a]
listaLT (Fork esq dir) = listaLT esq ++ listaLT dir

--c
ltHeight :: LTree a -> Int
ltHeight (Tip _) = 0
ltHeight (Fork esq dir) = 1 + max (ltHeight esq) (ltHeight dir)
