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