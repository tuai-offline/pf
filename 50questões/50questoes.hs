-- Questão 1

myenumFromTo :: Int -> Int -> [Int]
myenumFromTo x y    | x >= y = x : []
                    | x < y = x : myenumFromTo (x+1) y
                    | otherwise = []


-- TODO Questão 2 - Está a dar o 11 quando nao devia (usar exemplo)

myenumFromThenTo :: Int -> Int -> Int -> [Int]
myenumFromThenTo x y z  | x >= z = []
                        | x < z = x : myenumFromThenTo y ( y + ( y - x ) ) z
                        | otherwise = []


-- ! Questão 3 - Rever, conceitos não percebidos

(+++) :: [a] -> [a] -> [a]
(+++) x [] = x
(+++) [] x = x
(+++) (x:xs) l = x : ((+++) xs l)


-- Questão 4

posicao :: [a] -> Int -> a
posicao (h:t) 0 = h
posicao (h:t) x = posicao t (x-1)


-- Questão 5
reverso :: [a] -> [a]
reverso [] = []
reverso (h:t) = reverso t ++ [h]
