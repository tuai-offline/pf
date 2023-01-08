import Data.Char
import Data.List

dobros :: [Float] -> [Float]
dobros [] = []
dobros (h:t) = h*2 : dobros t

numOcorre :: Char -> String -> Int
numOcorre x [] = 0
numOcorre x (h:t) = if x == h then 1 + numOcorre x t
                    else numOcorre x t

positivos :: [Int] -> Bool
positivos (h:t) = if h > 0 then True
                  else False

soPos :: [Int] -> [Int]
soPos [] = []
soPos (h:t) = if h > 0 then h : soPos t
              else soPos t

somaNeg :: [Int] -> Int
somaNeg [] = 0
somaNeg (h:t) = if h < 0 then h + somaNeg t
                else somaNeg t

tresUlt :: [a] -> [a]
tresUlt (h:t) = if length (h:t) > 3 then tresUlt t
                else (h:t)

segundos :: [(a,b)] -> [b]
segundos [] = []
segundos ((x,xs):t) = xs : segundos t

nosPrimeiros :: (Eq a) => a -> [(a,b)] -> Bool
nosPrimeiros y ((x,xs):t) = if x == y then True
                            else False

sumTriplos :: (Num a, Num b, Num c) => [(a,b,c)] -> (a,b,c)
sumTriplos [] = (0,0,0)
sumTriplos ((x,y,z):t) = (x+a,y+b,z+c)
    where (a,b,c) = sumTriplos t

type Polinomio = [Monomio]
type Monomio = (Float,Int)

conta :: Int -> Polinomio -> Int
conta _ [] = 0
conta n ((x,xs):t)= if n == xs then 1 + conta n t
                    else conta n t

grau :: Polinomio -> Int
grau [] = 0
grau ((x,xs):t) = grauAux xs t

grauAux :: Int -> Polinomio -> Int
grauAux g [] = g
grauAux g ((x,xs):t) = if xs > g then grauAux xs t
                       else grauAux g t

selgrau :: Int -> Polinomio -> Polinomio
selgrau _ [] = []
selgrau g ((x,xs):t) = if g == xs then (x,xs) : selgrau g t
                       else selgrau g t

deriv :: Polinomio -> Polinomio
deriv [] = []
deriv ((x,xs):t) | xs == 0 = deriv t
                 | xs > 0 = (x*fromIntegral(xs),xs-1) : deriv t

-- ! ordena :: Polinomio -> Polinomio
-- ! ordena [] = []
-- ! ordena ((x,xs):t) = insere (x,xs) (ordena t)
-- !     where insere (x,xs) [] = (x,xs)
-- !           insere (x,xs) ((a,b):c) = if xs <= b then ((x,xs):(a,b):c)
-- !                                     else (a,b) : insere (x,xs) c
