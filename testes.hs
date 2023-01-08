type Ponto = (Float,Float)
data Retangulo = Rect Ponto Ponto

areaTotal :: [Retangulo] -> Float
areaTotal [] = 0
areaTotal (Rect (x1,y1) (x2,y2):t) = abs(x2-x1) * abs(y2-y1) + areaTotal t

insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (h:t) = if x <= h then x : (h:t)
                 else h : insert x t
