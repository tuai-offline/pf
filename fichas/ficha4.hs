import Data.Char

-- Questão 1
digitAlpha :: String -> (String,String)
digitAlpha [] = ([],[])
digitAlpha " " = (" ", "")
digitAlpha (x:xs) | isDigit x = (ls,x:ds)
                  | isAlpha x = (x:ls,ds)
                  | otherwise = (ls,ds)
    where (ls,ds) = digitAlpha xs

--Questão 2
nzp :: [Int] -> (Int,Int,Int)
nzp [] = (0,0,0)
nzp (x:xs) | x < 0 = (nn+1,nz,np)
           | x == 0 = (nn,nz+1,np)
           |x > 0 = (nn,nz,np+1)
    where (nn,nz,np) = nzp xs

divMod :: Int a => a -> a -> (a,a)
