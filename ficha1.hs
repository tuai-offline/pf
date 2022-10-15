import Data.Char

-- exercício 3

type Hora = (Int,Int)

meiaNoiteEUmQuarto :: Hora
meiaNoiteEUmQuarto = (0,15)

duasMenosUmQuarto :: Hora
duasMenosUmQuarto = (13,45)

-- exercício 3 ; alínea a)

horaValida :: Hora -> Bool
horaValida (h,m) = h >=0 && h < 24 && m >= 0 && m < 60

-- exercício 3 ; alínea b)

comparaHora :: Hora -> Hora -> Bool
comparaHora (h1,m1) (h2,m2) | h1 > h2  = True
                            | h1 == h2 = m1 > m2
                            | h1 < h2  = False

-- exercício 3 ; alínea c)

horasParaMinutos :: Hora -> Int
horasParaMinutos (h,m) = h * 60 + m

-- exercício 3 ; alínea d)

minutosParaHoras :: Int -> Hora 
minutosParaHoras m = (div m 60 , mod m 60)

-- exercício 3 ; alínea e)

diferencaHoras :: Hora -> Hora -> Int
diferencaHoras h1 h2 = abs ((horasParaMinutos h1) - (horasParaMinutos h2))

-- exercício 3 ; alínea f)

adicionaMinutos :: Int -> Hora -> Hora
adicionaMinutos m h = minutosParaHoras (m + horasParaMinutos h)


-- exercício 5

data Semaforo = Verde 
              | Amarelo
              | Vermelho
              deriving (Show, Eq)

-- exercício 5 ; alínea a)

next :: Semaforo -> Semaforo
next Vermelho = Amarelo
next Amarelo = Vermelho
next Verde = Vermelho

-- exercício 5 ; alínea b)

stop :: Semaforo -> Bool
stop Vermelho = True
stop _        = False

-- exercício 5 ; alínea c)

safe :: Semaforo -> Semaforo -> Bool
safe _ Vermelho = True
safe Vermelho _ = True
safe _ _        = False


-- exercício 6

data Figura = Cartesiano Double Double
           | Retangulo  Ponto  Ponto
           | Triangulo  Ponto  Ponto  Ponto
           deriving (Show,Eq)

data Ponto = Cartesiano Double Double
           | Polar      Double Double
           deriving (Show,Eq)

circ :: Figura
circ = Circulo (Cartesiano 0 0) 2.3

rect :: Figura
rect = Retangulo (Cartesiano 0 0) (Cartesiano 2 2)

poligono :: Figura -> Bool
poligono (circulo p r) = False
poligono _             = True

vertices (Triangulo v1 v2 v3) = [v1,v2,v3]
vertices (Retangulo (Cartesiano a b) (Cartesiano x y)) =
    [ Cartesiano a b
    , Cartesiano a y
    , Cartesiano x y
    , Cartesiano x b
    ]

dist' :: Ponto -> Ponto -> Double
dist' (Cartesiano x1 x2) (Cartesiano y1 y2)
        = sqrt ( (x2-x1)^2 + (y2-y1)^2 )

area :: Figura -> Double
area (Triangulo p1 p2 p3) =
    let a = dist' 
