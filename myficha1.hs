--1
--1 (a)
perimetro :: Double -> Double
perimetro r = 2*pi*r

--1 (b)
dist :: (Double,Double) -> (Double,Double) -> Double
dist (x1,y1) (x2,y2) = sqrt(x-y)
    where   x = (x2-x1)^2
            y = (y2-y1)^2