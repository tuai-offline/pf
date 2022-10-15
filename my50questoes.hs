-- Questão 1
myEnumFromTo :: Int -> Int -> [Int]
myEnumFromTo x y | x >= y = []
                 | x < y = x : myEnumFromTo (x+1) y
                 | otherwise = []

-- Questão 2
myEnumFromThenTo :: Int -> Int -> Int -> [Int]
myEnumFromThenTo x y z | x >= z = []
                       | x < z = x : myEnumFromThenTo y (y+(y-x)) z
                       | otherwise = []

-- Questão 3
concatenar :: [a] -> [a] -> [a]
concatenar l [] = l
concatenar [] l = l
concatenar (h:t) l = h : concatenar t l

-- Questão 4
posicao :: [a] -> Int -> a
posicao (h:t) 0 = h
posicao (h:t) x = posicao t (x-1)

-- Questão 5
reverso :: [a] -> [a]
reverso [] = []
reverso (h:t) = reverso t ++ [h]

-- !Questão 6
myTake :: Int -> [a] -> [a]
myTake 0 _ = []
myTake _ [] = []
myTake x (h:t) = if x>0 then h : myTake (x-1) t
                 else []

-- TODO Questão 7 - Github do Quintela tem erro, corrigir segunda-feira
myDrop :: Int -> [a] -> [a]
myDrop 0 l = l
myDrop _ [] = []
myDrop n (h:t) = if n < 0 then (h:t)
                 else myDrop (n-1) t

-- Questão 8
myZip :: [a] -> [b] -> [(a,b)]
myZip _ [] = []
myZip [] _ = []
myZip (h:t) (x:xs) = [(h,x)] ++ myZip t xs

-- Questão 9
myReplicate :: Int -> a -> [a]
myReplicate 0 _ = []
myReplicate n x = if n < 0 then []
                  else x : myReplicate (n-1) x

-- Questão 10 
myIntersperse :: a -> [a] -> [a]
myIntersperse hdsajfdhsajdsajn
