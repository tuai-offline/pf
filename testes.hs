isPrime :: Int -> Bool
isPrime 1 = False
isPrime x = if numeroDeDivisores x 2 == 0 then True
            else False

numeroDeDivisores :: Int -> Int -> Int
numeroDeDivisores x y | y >= x = 0
                      | mod x y == 0 = 1
                      | otherwise = numeroDeDivisores x (y+1)
