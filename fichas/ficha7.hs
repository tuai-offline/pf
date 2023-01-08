data ExpInt = Const Int
            | Simetrico ExpInt
            | Mais ExpInt ExpInt
            | Menos ExpInt ExpInt
            | Mult ExpInt ExpInt

e1 :: ExpInt
e1 = Mais (Const 3) (Mult (Const 4) (Const 2))

e2 = Mult e1 e1

calcula :: ExpInt -> Int
calcula (Const i) = i
calcula (Simetrico e) = - calcula e
calcula (Mais e1 e2) = calcula e1 + calcula e2
calcula (Menos e1 e2) = calcula e1 - calcula e2
calcula (Mult e1 e2) = calcula e1 * calcula e2

infixa :: ExpInt -> String
infixa (Const i) = show i
infixa (Simetrico e) = "-(" ++ infixa e ++ ")"
infixa (Mais e1 e2) = "(" ++ infixa e1 ++ " + " ++ infixa e2 ++ ")"
infixa (Menos e1 e2) = "(" ++ infixa e1 ++ " - " ++ infixa e2 ++ ")"
infixa (Mult e1 e2) = "(" ++ infixa e1 ++ " * " ++ infixa e2 ++ ")"

posfixa :: ExpInt -> String
posfixa (Const i) = show i
posfixa (Simetrico e) = "-" ++ posfixa e
posfixa (Mais e1 e2) = posfixa e1 ++ " " ++ posfixa e2 ++ " +"
posfixa (Menos e1 e2) = posfixa e1 ++ " " ++ posfixa e2 ++ " -"
posfixa (Mult e1 e2) = posfixa e1 ++ " " ++ posfixa e2 ++ " *"



data RTree a = R a [RTree a]
             deriving Show

rt :: RTree Int
rt = R 7 [ R 3 [ R 2  []
               , R 14 []]
         , R 5 []
         , R 10 [ R 17 [ R 44 []
                       , R 6  []]
                ]
         ]

soma :: Num a => RTree a -> a
soma (R v filhos) = v + sum (map soma filhos)

altura :: RTree a -> Int
altura (R a []) = 1
altura (R a filhos) = 1 + maximum(map altura filhos)

prune :: Int -> RTree a -> RTree a
prune l (R v _) = R v []
prune n (R v filhos) = R v (map (prune (n-1)) filhos)

