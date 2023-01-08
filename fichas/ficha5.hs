type Polinomio = [Monomio]
type Monomio = (Float,Int)

p :: Polinomio
p = [(2,3), (3,4), (5,3), (4,5)]

selgrau :: Int -> Polinomio -> Polinomio
selgrau n [] = []
selgrau n ((c,e):t) | e == n = (c,e) : selgrau n t
                    | otherwise = selgrau n t

selgrau' n p = filter f p
    where f (c,e) = e == n

selgrau'' n p = filter (\ (c,e) -> e == n) p

conta :: Int -> Polinomio -> Int
conta n p = length (selgrau'' n p)

calcula :: Float -> Polinomio -> Float
calcula x p = sum (map (\(c,e) -> c * x^e) p)

calcula' x p = foldr (+) 0 (map (\(c,e) -> c * x^e) p)
