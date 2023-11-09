--1
data BTree a = Empty | Node a (BTree a) (BTree a)
          deriving Show
--a
altura :: BTree a -> Int
altura Empty = 0
altura (Node a esq dir) = 1 + max (altura esq) (altura dir)

--b
contaNodos :: BTree a -> Int
contaNodos Empty = 0
contaNodos (Node a esq dir) = 1 + contaNodos esq + contaNodos dir

--c
folhas :: BTree a -> Int
folhas Empty = 0
folhas (Node a Empty Empty) = 1
folhas (Node a esq dir) = folhas esq + folhas dir

--d
prune :: Int -> BTree a -> BTree a
prune _ Empty = Empty
prune x (Node a esq dir) | x == 0 = Node a Empty Empty
                         | otherwise = Node a (prune (x-1) esq) (prune (x-1) dir)

--e
path :: [Bool] -> BTree a -> [a]
path _ Empty = []
path [] (Node a esq dir) = [a]
path (d:ds) (Node a esq dir) | d == False = a : path ds esq
                             | otherwise = a : path ds dir

--f
mirror :: BTree a -> BTree a
mirror Empty = Empty
mirror (Node a esq dir) = Node a (mirror dir) (mirror esq)

--g
zipWithBT :: (a -> b -> c) -> BTree a -> BTree b -> BTree c
zipWithBT _ _ Empty = Empty
zipWithBT _ Empty _ = Empty
zipWithBT f (Node a1 esq1 dir1) (Node a2 esq2 dir2) = Node (f a1 a2) (zipWithBT f esq1 esq2) (zipWithBT f dir1 dir2)

--h
unzipBT :: BTree (a,b,c) -> (BTree a,BTree b,BTree c)
unzipBT (Node (a,b,c) esq dir) = (Node a e1 d1, Node b e2 d2, Node c e3 d3)
    where (e1,e2,e3) = unzipBT esq
          (d1,d2,d3) = unzipBT dir

--2
--a
minimo :: Ord a => BTree a -> a
minimo (Node a esq dir) = case esq of
                          Empty -> a
                          Node r _ _ -> minimo esq

--b
semMinimo :: Ord a => BTree a -> BTree a
semMinimo (Node a Empty Empty) = Empty
semMinimo (Node a Empty dir) = dir
semMinimo (Node a esq dir) = Node a (semMinimo esq) dir

--c
minSmin :: Ord a => BTree a -> (a,BTree a)
minSmin (Node a Empty Empty) = (a, Empty)
minSmin (Node a Empty dir) = (a, dir)
minSmin (Node a esq dir) = (minimo esq, Node a (semMinimo esq) dir)

--d
remove :: Ord a => a -> BTree a -> BTree a
remove x (Node a esq dir) | x > a = Node a esq (remove x dir)
                          | x < a = Node a (remove x esq) dir
                          | otherwise = aux x (Node a esq dir)

aux :: Ord a => a -> BTree a -> BTree a
aux x (Node a esq dir) = case esq of
                         Empty -> dir
                         _ -> case dir of
                                      Empty -> esq
                                      _ -> Node m esq d
                                        where (m,d) = minSmin dir

--3
type Aluno = (Numero,Nome,Regime,Classificacao)
type Numero = Int
type Nome = String
data Regime = ORD | TE | MEL deriving Show
data Classificacao = Aprov Int | Rep | Faltou
  deriving Show
type Turma = BTree Aluno -- arvore binaria de procura (ordenada por numero)

--turma para testes retirada do github da RisingFisan
turma1 :: Turma
turma1 = (Node (15,"Luís",TE,Aprov 14) (Node (12,"Joana",MEL,Faltou) (Node (7,"Diogo",TE,Rep) Empty
                                                                                               Empty) 
                                                                      (Node (14,"Lara",ORD,Aprov 19) Empty
                                                                                                     Empty))
                                        (Node (20,"Pedro",TE,Aprov 10) Empty
                                                                       (Node (25,"Sofia",ORD,Aprov 20) (Node (23,"Rita",ORD,Aprov 17) Empty
                                                                                                                                      Empty)
                                                                                                       (Node (28,"Vasco",MEL,Rep) Empty
                                                                                                                                  Empty))))

--a
inscNum :: Numero -> Turma -> Bool
inscNum _ Empty = False
inscNum n (Node (num,_,_,_) esq dir) | n > num = inscNum n dir
                                     | n < num = inscNum n esq
                                     | otherwise = True

--b
inscNome :: Nome -> Turma -> Bool
inscNome _ Empty = False
inscNome n (Node (_,nome,_,_) esq dir) | n == nome = True
                                       | otherwise = inscNome n esq || inscNome n dir

--c
trabEst :: Turma -> [(Numero,Nome)]
trabEst Empty = []
trabEst (Node (num,nome,reg,_) esq dir) = case reg of
                                          TE -> trabEst esq ++ [(num,nome)] ++ trabEst dir
                                          _ -> trabEst esq ++ trabEst dir

--d
nota :: Numero -> Turma -> Maybe Classificacao
nota _ Empty = Nothing
nota x (Node (num,_,_,classi) esq dir) | x > num = nota x dir
                                       | x < num = nota x esq
                                       | otherwise = Just classi

--e
percFaltas :: Turma -> Float
percFaltas Empty = 0.0
percFaltas turma = (faltas / alunos) * 100
  where faltas = fromIntegral (numFaltas turma)
        alunos = fromIntegral (numAlunos turma)

numAlunos :: Turma -> Int
numAlunos Empty = 0
numAlunos (Node a esq dir) = 1 + numAlunos esq + numAlunos dir

numFaltas :: Turma -> Int
numFaltas Empty = 0
numFaltas (Node (_,_,_,classi) esq dir) = case classi of
                                          Faltou -> 1 + numFaltas esq + numFaltas dir
                                          _ -> numFaltas esq + numFaltas dir

--f
mediaAprov :: Turma -> Float
mediaAprov turma = notasAlunosPassaram/numAlunosPassaram
  where notasAlunosPassaram = fromIntegral (notasAlunosAprov turma) :: Float
        numAlunosPassaram = fromIntegral (numAlunosAprov turma) :: Float

numAlunosAprov :: Turma -> Int
numAlunosAprov Empty = 0
numAlunosAprov (Node (_,_,_,classi) esq dir) = case classi of
                                               Aprov _ -> 1 + numAlunosAprov esq + numAlunosAprov dir
                                               _ -> numAlunosAprov esq + numAlunosAprov dir

notasAlunosAprov :: Turma -> Int
notasAlunosAprov Empty = 0
notasAlunosAprov (Node (_,_,_,classi) esq dir) = case classi of
                                                 Aprov x -> x + notasAlunosAprov esq + notasAlunosAprov dir
                                                 _ -> notasAlunosAprov esq + notasAlunosAprov dir

--g (apenas consegui fazer com que a funcao atravessa-se uma vez a arvore para cada funcao)
aprovAv :: Turma -> Float
aprovAv turma = fromIntegral (numAlunosAprov turma) / numAvaliados turma

numAvaliados :: Turma -> Float
numAvaliados Empty = 0.0
numAvaliados (Node (_,_,_,classi) esq dir) = case classi of
                                             Aprov _ -> 1 + numAvaliados esq + numAvaliados dir
                                             Rep -> 1 + numAvaliados esq + numAvaliados dir
                                             _ -> numAvaliados esq + numAvaliados dir
