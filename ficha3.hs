import Ficha1

type Etapa = (Hora2,Hora2)
type Viagem = [Etapa]

--a
bemConstruido :: Etapa -> Bool
bemConstruido (h1,h2) = ((valida2 h1) && (valida2 h2)) && (depois h2 h1)

--b
viagemBemConstruida :: Viagem -> Bool
viagemBemConstruida [] = True
viagemBemConstruida [h] = bemConstruido h
viagemBemConstruida ((h1,h2):(h3,h4):t) = bemConstruido (h1,h2) && bemConstruido (h2,h3) && viagemBemConstruida ((h3,h4):t)

--c
partidaEchegada :: Viagem -> (Hora2,Hora2)
partidaEchegada [(x,y)] = (x,y)
partidaEchegada (h1:t) = (fst h1,snd (last t))

--d
tempoDeViagem :: Viagem -> Hora2
tempoDeViagem [(h1,h2)] = diferencaHora h1 h2
tempoDeViagem ((h1,h2):t) = adiciona (horaParaMin (diferencaHora h1 h2)) (tempoDeViagem t) 

--e
tempoDeEspera :: Viagem -> Hora2
tempoDeEspera l =  diferencaHora (tempoTotalDeViagem l)  (tempoDeViagem l) 

--Ou
tempoDeEspera2 :: Viagem -> Hora2
tempoDeEspera2 ((e1,e2):(e3,e4):t) = adiciona (diferencaHora e2 e3) (horaParaMin (tempoDeEspera2 (e3,e4):t))

--f
tempoTotalDeViagem :: Viagem -> Hora2
tempoTotalDeViagem ((h1,h2):t) = diferencaHora h1 (snd (last t))

--5
data Movimento = Credito Float | Debito Float
               deriving Show

data Data = D Int Int Int
          deriving Show

data Extracto = Ext Float [(Data,String,Movimento)]
              deriving Show

--a
extValor :: Extracto -> Float -> [Movimento]
extValor (Ext _ []) _ = []
extValor (Ext x1 ((a,b,c):t)) x2 | x1 > x2 = c : extValor (Ext x1 t) x2
                                 | otherwise = extValor (Ext x1 t) x2

-- b
filtro :: Extrato -> [String] -> [(Data,Movimento)]
filtro (Ext _ mvs) descs = filtroMovimentos mvs descs

filtroMovimentos :: [(Data,String,Movimento)] -> [String] -> [(Data,Movimento)]
filtroMovimentos ((d,desc,m):t) descs | elem desc descs = 
