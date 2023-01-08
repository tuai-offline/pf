-- Questão 1
data BTree a = Empty
             | Node a (BTree a) (BTree a)
             deriving Show

arv :: BTree Int
arv = Node 7 (Node 5 (Node 3 Empty Empty)
                     (Node 2 Empty Empty))
             (Node 6 (Node 1 Empty (Node 8 Empty Empty)) Empty)

-- Questão 1 - Alínea a)
altura :: BTree a -> Int
altura Empty = 0
altura (Node _ e d) = 1 + max (altura e) (altura d)

-- Questão 1 - Alínea b)
contaNodes :: BTree a -> Int
contaNodes Empty = 0
contaNodes (Node _ e d) = 1 + contaNodes e + contaNodes d

-- Questão 1 - Alínea c)
folhas :: BTree a -> Int
folhas Empty = 0
folhas (Node _ Empty Empty) = 1
folhas (Node _ e d) = folhas e + folhas d

-- Questão 1 -- Alínea d)
prune :: Int -> BTree a -> BTree a
prune 0 _ = Empty
prune x Empty = Empty
prune x (Node r e d) = Node r (prune (x-1) e) (prune (x-1) d)

-- Questão 1 -- Alínea e)
path :: [Bool] -> BTree a -> [a]
-- ? False: esquerda , True: direita
path l Empty = []
path [] (Node x _ _) = [x]
path (h:t) (Node x e d) | h == True = x : path t d
                        | otherwise = x : path t e

-- Questão 1 - Alínea f)
mirror :: BTree a -> BTree a
mirror Empty = Empty
mirror (Node i e d) = Node i (mirror d) (mirror e)

-- Questão 1 - Alínea g)
zipWithBT :: (a -> b -> c) -> BTree a -> BTree b -> BTree c
zipWithBT f _ Empty = Empty
zipWithBT f Empty _ = Empty
zipWithBT f (Node i1 e1 d1) (Node i2 e2 d2)
        = Node (f i1 i2) (zipWithBT f e1 e2) (zipWithBT f d1 d2)

{-
arv :: BTree Int
arv = Node (7,"Ana",17)
            (Node (5,"Rui",10)
                    (Node (3,"Ze",14) Empty Empty)
                    (Node (2,"Joao",18) Empty Empty)
            (Node (6,"To",13)
                    (Node (1,"Maria",19) Empty))
                    Empty)

unzipBT :: BTree (a,b,c) -> (BTree a, BTree b, BTree c)
unzipBT Empty = (Empty, Empty, Empty)
unzipBT (Node (a,b,c) e d) = ( , , )
    where ( e1 , e2 , e3 ) = unzipBT e
          ( d1 , d2 , d3 ) = unzipBT d
-}

-- Questão 3
type Aluno = (Numero, Nome, Regime, Classificacao)
type Numero = Int
type Nome = String
data Regime = ORD | TE | MEL deriving Show
data Classificacao = Aprov Int
                   | Rep
                   | Faltou
    deriving Show
type Turma = BTree Aluno -- árvore binária de procura (ordenada por número)


-- árvore retirada do perfil RisingFisan
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


-- Questão 3 - Alínea a)
inscNum :: Numero -> Turma -> Bool 
inscNum _ Empty = False
inscNum n (Node (num,_,_,_) e d) | n > num = inscNum n d
                                 | n < num = inscNum n e
                                 |otherwise = True

-- Questão 3 - Alínea b)
inscNome :: Nome -> Turma -> Bool
inscNome _ Empty = False
inscNome n (Node (_,nome,_,_) e d) | n == nome = True
                                   | otherwise = inscNome n e || inscNome n d

-- Questão 3 - Alínea c)
trabEst :: Turma -> [(Numero,Nome)]
trabEst Empty = []inscNum n (Node (num,_,_,_) e d) | n > num = inscNum n d

trabEst (Node (n,no,TE,_) e d) = (trabEst e) ++ [(n,no)] ++ (trabEst d)
trabEst (Node i e d) = trabEst e ++ trabEst d

-- Questão 3 - Alíena d)
nota :: Numero -> Turma -> Maybe Classificacao
nota n Empty = Nothing
nota n (Node (num,_,_,classi) e d) | n > num = nota n d
                                   | n < num = nota n e
                                   | otherwise = Just classi

-- Questão 3 - Alínea e)
percFaltas :: Turma -> Float
percFaltas t = let (a,f) = avalFaltas t
               in f/(a+f)
        where avalFaltas :: Turma -> (Int,Int) -- aval, falt
              avalFaltas Empty = (0,0)
              avalFaltas (Node (_,_,_,Faltou) e d) = (ae+ad , fe+fd+1)
                where 
              avalFaltas (Node i e d) = (ae+ad , fe+fd)
                where 