numOcorre :: Char -> String -> Int
numOcorre x [] = 0
numOcorre x (h:t) = if x == h then 1 + numOcorre x t
                    else numOcorre x t
