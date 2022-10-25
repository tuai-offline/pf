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

-- Questão 7
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

-- TODO Questão 10 - Fiz de forma diferente e meio cansada. Perguntar se está mal
myIntersperse :: a -> [a] -> [a]
myIntersperse x (h:t) = if length (h:t) <= 1 then h : []
                        else h : x : myIntersperse x t

-- ! Questão 11
myGroup :: Eq a => [a] -> [[a]]
myGroup [] = [[]]
myGroup [h] = [[h]]
myGroup (h:t) = let (x:xs):y = myGroup t
                in if h==x then (h:x:xs):y
                else [h]:(x:xs):y

-- Questão 12
myConcat :: [[a]] -> [a]
myConcat [] = []
myConcat (h:t) = h ++ myConcat t

-- ! Questão 13
myInits :: [a] -> [[a]]
myInits [] = [[]]
myInits l = myInits(myInit l) ++ [l]

myInit :: [a] -> [a]
myInit [x] = []
myInit (h:t) = h : myInit t

-- Questão 14
myTails :: [a] -> [[a]]
myTails [] = []
myTails (h:t) = [(h:t)] ++ myTails t

-- Questão 15
myHeads :: [[a]] -> [a]
myHeads [] = []
myHeads ([]:t) = myHeads t
myHeads ((x:xs):t) = x : myHeads t

-- ! Questão 16
myTotal :: [[a]] -> Int
myTotal [[]] = 0
myTotal ([]:t) = myTotal t
myTotal ((x:xs):t) = 1 + myTotal (xs:t)

-- Questão 17
fun :: [(a,b,c)] -> [(a,c)]
fun [] = []
fun ((a,b,c):t) = (a,c) : fun t

-- Questão 18
cola :: [(String,b,c)] -> String
cola [] = []
cola ((a,b,c):t) = a ++ cola t

-- Questão 19
idade :: Int -> Int -> [(String,Int)] -> [String]
idade _ _ [] = []
idade x y ((a,b):t) = if x - b >= y then a : idade x y t
                      else idade x y t

-- Questão 20
powerEnumFrom :: Int -> Int -> [Int]
powerEnumFrom n 0 = []
powerEnumFrom n m = powerEnumFrom n (m-1) ++ [n^(m-1)]

-- TODO Questão 21 - Pedir ajuda 
{-isPrime :: Int -> Bool
isPrime 1 = False
isPrime

aux ::
-} 

-- Questão 22
isPrefixOf :: Eq a => [a] -> [a] -> Bool
isPrefixOf [] l = True
isPrefixOf (x:xs) (h:t) = if x == h then isPrefixOf xs t 
                          else False 

-- Questão 23 
isSuffixOf :: Eq a => [a] -> [a] -> Bool
isSuffixOf l [] = False
isSuffixOf [] l = True
isSuffixOf (x:xs) (h:t) = if x == h then isSuffixOf  xs t
                          else isSuffixOf (x:xs) t

-- Questão 24
isSubsequenceOf :: Eq a => [a] -> [a] -> Bool
isSubsequenceOf  [] [] = True
isSubsequenceOf l [] = False
isSubsequenceOf (x:xs) (h:t) = if x == h then isSubsequenceOf  xs t
                               else isSubsequenceOf (x:xs) t

-- Questão 25 
elemIndices :: Eq a => a -> [a] -> [Int]
elemIndices x l = eI x l 0

eI :: Eq a => a -> [a] -> Int -> [Int]
eI _ [] _ = []
eI x (h:t) p = if x == h then p : eI x t (p+1)
               else eI x t (p+1)

-- Questão 26
nub :: Eq a => [a] -> [a]
nub [] = []
nub (h:t) = if pertence h t then nub t
            else h : nub t

pertence :: Eq a => a -> [a] -> Bool
pertence x [] = False
pertence x (h:t) = if x == h then True
                   else pertence x t

-- Questão 27
delete :: Eq a => a -> [a] -> [a]
delete _ [] = []
delete x (h:t) = if x == h then t
                 else h : delete x t

-- ! Questão 28
barra :: Eq a => [a] -> [a] -> [a]
barra [] _ = []
barra l [] = l
barra (x:xs) (h:t) = barra (delete x (h:t)) xs

-- ! Questão 29
union :: Eq a => [a] -> [a] -> [a]
union [] l = l
union l [] = l
union (x:xs) (h:t) = if auxUnion x (h:t) then union (x:xs) t
                     else x : union xs (h:t)

auxUnion :: Eq a => a -> [a] -> Bool
auxUnion x [] = False
auxUnion x  (h:t) = if x == h then True
                    else auxUnion x t

-- Questão 30
myIntersect :: Eq a => [a] -> [a] -> [a]
myIntersect [] l = []
myIntersect l [] = []
myIntersect (x:xs) (h:t) = if auxIntersect x (h:t) then x : myIntersect xs (h:t)
                           else myIntersect xs (h:t)

auxIntersect :: Eq a => a -> [a] -> Bool
auxIntersect x [] = False
auxIntersect x (h:t) = if x == h then True
                       else auxIntersect x t

-- Questão 31
myInsert :: Ord a => a -> [a] -> [a]
myInsert x [] = [x]
myInsert x (h:t) = if x > h then h : myInsert x t
                   else x : (h:t)

-- Questão 32
myUnwords :: [String] -> String
myUnwords [] = []
myUnwords [x] = x
myUnwords (h:t) = h ++ " " ++ myUnwords t

-- Questão 33
myUnlines :: [String] -> String
myUnlines [] = []
myUnlines [x] = x
myUnlines (h:t) = h ++ "\n" ++ myUnlines t

-- Questão 34
myPMaior :: Ord a => [a] -> Int
myPMaior [x] = 0
myPMaior (h:t) = auxPMaior 0 h t

auxPMaior :: Ord a => Int -> a -> [a] -> Int
auxPMaior acc _ [] = acc
auxPMaior acc x (h:t) = if x >= h then auxPMaior (acc+1) x t
                         else auxPMaior(acc+1) x t
