perimetro :: Double -> Double
perimetro r = r*2*pi

dist :: (Double,Double) -> (Double,Double) -> Double
dist (x1,y1) (x2,y2) = sqrt(x-y)
    where x = (x2-x1)^2
          y = (y2-y1)^2

primUlt :: [a] -> (a,a)
primUlt (h:t) = (h,last(h:t))

multiplo :: Int -> Int -> Bool
multiplo m n = if mod m n == 0 then True
               else False

truncaImpar :: [a] -> [a]
truncaImpar [] = []
truncaImpar (h:t) = if mod (length (h:t)) 2 /= 0 then t
                    else (h:t)

max2 :: Int -> Int -> Int
max2 x y = if x > y then x
           else y

max3 :: Int -> Int -> Int -> Int
max3 x y z = max2 (max2 x y) z

-- ! nRaizes :: 

-- ! raizes :: 

type Hora = (Int,Int)

horaValida :: Hora -> Bool
horaValida (h,m) = if (h >= 0 && h < 24) && (m >= 0 && m < 60) then True
                   else False

horaCompara :: Hora -> Hora -> Bool
horaCompara (h1,m1) (h2,m2) | h1 > h2 = True
                            | h1 == h2 && m1 > m2 = True
                            | otherwise = False

horasParaMinutos :: Hora -> Int
horasParaMinutos (h,m) = h * 60 + m

minutosParaHoras :: Int -> Hora
minutosParaHoras m = (div m 60,mod m 60)

diferencaHoras :: Hora -> Hora -> Int
diferencaHoras (h1,m1) (h2,m2) = abs(horasParaMinutos (h1,m1) - horasParaMinutos (h2,m2))

adicionarMinutos :: Int -> Hora -> Hora
adicionarMinutos x (h,m) = minutosParaHoras (x + y)
    where y = horasParaMinutos (h,m)

data HoraNova = H Int Int deriving (Show,Eq)

horaValidaNova :: HoraNova -> Bool
horaValidaNova (H h m) = if (h >= 0 && h < 24) && (m >= 0 && m < 60) then True
                   else False

data Semaforo = Verde | Amarelo | Vermelho deriving (Show,Eq)

next :: Semaforo -> Semaforo
next x | x == Verde = Amarelo
       | x == Amarelo = Vermelho
       | x == Vermelho = Verde

stop :: Semaforo -> Bool
-- True = obrigatório parar
stop x | x == Verde = False
       | x == Amarelo = False
       | x == Vermelho = True

safe :: Semaforo -> Semaforo -> Bool
-- True = seguro
safe x y | x == Verde && y == Verde = False
         | x == Verde && y == Amarelo = False
         | x == Amarelo && y == Amarelo = False 
         | x == Amarelo && y == Verde = False
         | otherwise = True

data Ponto = Cartesiano Double Double | Polar Double Double
             deriving (Show,Eq)

posx :: Ponto -> Double
posx (Cartesiano x y) = abs(x)
posx (Polar d a) = abs(d * (cos a))

posy :: Ponto -> Double
posy (Cartesiano x y) = abs(y)
posy (Polar d a) = abs(d + (sin a))

data Figura = Circulo Ponto Double
            | Retangulo Ponto Ponto
            | Triangulo Ponto Ponto
              deriving (Show,Eq)

-- TODO Fazer a partir do 7
