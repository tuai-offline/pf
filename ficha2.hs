-- Questão 2

-- Exemplo
somatorio :: [Int] -> Int
somatorio [] = 0
somatorio (h:t) = h + somatorio t

-- Questão 2 ; alínea (a)
dobros :: [Float] -> [Float]
dobros [] = []
dobros (h:t) = 2*h : dobros t

-- Questão 2 ; alínea (b)
numOcorre :: Char -> String -> Int
numOcorre x [] = 0
numOcorre x (h:t) = if x == h then 1 + numOcorre x t
                    else numOcorre x t

-- Questão 2 ; alínea (c)
positivos :: [Int] -> Bool
positivos [x] = x >= 0
positivos (h:t) = if h >= 0 then positivos t
                  else False

-- Questão 2 ; alínea (d)
soPos :: [Int] -> [Int]
soPos [] = []
soPos (h:t) | h >=0 = h : soPos t 
            | otherwise = soPos t
    
-- Questão 2 ; alínea (e)


-- Questão 2 ; alínea (f)
tresUlt :: [a] -> [a]
tresUlt [] = []
tresUlt (x:t) | length(t) + 1 <= 3 = (x:t)
              | otherwise = tresUlt t

-- Questão 2 ; alínea (g)
segundos :: [(a,b)] -> [b]
segundos [] = []
segundos ((x,y):t) = y : segundos t

-- Questão 2 ; alínea (h)
sumTriplos :: (Num a, Num b, Num c) => [(a,b,c)] -> (a,b,c)
sumTriplos [(a,b,c)] = (a,b,c)
sumTriplos ((a,b,c):t) = let (a',b',c') = sumTriplos t
                         in (a+a', b+b', c+c')

type Polinomio = [Monomio]
type Monomio = (Float, Int)

p :: Polinomio
p = [(2,3), (3,4), (5,3), (4,5)]

conta :: Int -> Polinomio -> Int
conta n [] = 0
conta n ((c,e):t) | n == e = 1 + conta n t
                  | otherwise = conta n t

grau :: Polinomio -> Int 
grau [(c,e)] = e
grau ((c,e):t) = max e (grau t)

selgrau :: Int -> Polinomio -> Polinomio
selgrau _ [] = []
selgrau n ((x,xs):t) = if n == xs then (x,xs):selgrau n t
                      else selgrau n t


deriv :: Polinomio -> Polinomio
deriv [] = []
deriv ((a,0):t) = deriv t
deriv ((a,x):t) = (a*fromIntegral(x), x-1): deriv t

calcula :: Float -> Polinomio -> Float
calcula _ [] = 0
calcula x ((c,e):t) = ( c * (x^e) ) + calcula x t

somaMonomio :: Monomio -> Polinomio -> Polinomio
somaMonomio m [] = [m]
somaMonomio (c,e) ((c',e'):t) | e == e' = (c+c' ,e) : t
                            | otherwise = (c',e') : somaMonomio (c,e) t
