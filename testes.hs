-- 
-- group [] = []
-- group (x:y:xs) = if x == y then [x] ++ group (y:xs)
--                  else [x] : group (y:xs)
-- 
-- a : [a] : [b] : c : c : [c]

inits :: [a] -> [[a]]
inits [] = [[]]
inits (h:t) = inits (aux(h:t)) ++ [(h:t)]

aux :: [a] -> [a]
aux [x] = []
aux (h:t) = h : aux t