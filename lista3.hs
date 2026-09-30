import Data.Bits (toIntegralSized)
import Text.XHtml (h2)
data Hora = H Int Int
   deriving Show
type Etapa = (Hora,Hora)
type Viagem = [Etapa]

data Ponto = Cartesiano Double Double | Polar Double Double
  deriving (Show,Eq)

type Poligonal =[Ponto]

horaValida :: Hora -> Bool
horaValida (H x y )=  x >= 0 &&  x < 24 && y >= 0 && y < 60

horasParaMinutos :: Hora -> Int
horasParaMinutos (H h m) = h *60 + m

minutosParaHora :: Int -> Hora 
minutosParaHora m =  H (mod(div m 60) 24) (mod m 60)

comparaHora :: Hora -> Hora -> Bool
comparaHora (H h1 m1) (H h2 m2)
    | h1 < h2 = True
    | h1 == h2 && m1 < m2 = True
    | otherwise = False

etapaValida (hi,hc) = horaValida hi && horaValida hc && comparaHora hi hc

viagemValida :: Viagem -> Bool
viagemValida [e] = etapaValida e
viagemValida (e1:e2:t) =
    etapaValida e1 &&
    comparaHora (snd e1) (fst e2) &&
    viagemValida (e2:t)
---- serve para no inicio da viagem até a ultima chega ex [(9 30, 10 40) (14 10, 15,30)] ai o tempo é (9 30  15 30)
horaPartidaChegada :: Viagem -> (Hora, Hora)
horaPartidaChegada [] = error "Viagem vazia"
horaPartidaChegada [e] = e
horaPartidaChegada v
    | viagemValida v = (fst (head v), snd (last v))
    | otherwise      = error "Viagem inválida"


duracaoEtapa :: Etapa -> Int
duracaoEtapa (h1,hf) = horasParaMinutos hf - horasParaMinutos h1

tempoEmMovimentoMinutos :: Viagem ->Int
tempoEmMovimentoMinutos []    = 0
tempoEmMovimentoMinutos (h:t) = duracaoEtapa h + tempoEmMovimentoMinutos t

tempoEmMovimento :: Viagem -> Hora
tempoEmMovimento v = minutosParaHora(tempoEmMovimentoMinutos v)

tempoEmEspera :: Viagem -> Int
tempoEmEspera v =
    let (hi, hf) = horaPartidaChegada v
        tt  = duracaoEtapa (hi, hf)
        tem = tempoEmMovimentoMinutos v
    in tt - tem    

tempoTotal :: Viagem -> Hora
tempoTotal v = minutosParaHora duracaonovoototal 

        where 
        (hi, hf) = horaPartidaChegada v
        duracaonovoototal= duracaoEtapa (hi, hf) 


--------------- teste 1 haskell 2025
bub ::Ord a => [a] ->[a]
bub (x:y:ys) | x <= z       = x :z :zs
             | otherwise    = z : x : zs

            where (z:zs) = bub(y:ys)

bub l = l


data Contacto = Casa Integer
      | Trab Integer
      | Tlm Integer
      | Email String
        deriving Show


type Nome = String
type Agenda = [(Nome, [Contacto])]


acrescEmail :: Nome -> String -> Agenda -> Agenda
acrescEmail nome email [] = [(nome, [Email email])]
acrescEmail nome email ((n , contatos ) : resto) |  nome == n = (n , [Email email] ++ contatos ) : resto 
                                                         | otherwise = (n , contatos) : acrescEmail nome email resto 


agenda2 :: Agenda
agenda2 = [("Ana", [Email "ana@gmail.com"]), ("Bruno", [Trab 253123456])]

verEmails :: Nome -> Agenda -> Maybe [String]
verEmails _ [] = Nothing
verEmails nome ((n, contatos):t)
  | nome == n  = Just [ email | Email email <- contatos ]
  | otherwise  = verEmails nome t 

bubl :: Ord a => [a] -> [a]
bubl [] = []
bubl [x] = [x]
bubl (x:y:ys)
    | x > y     = y : bubl (x:ys)
    | otherwise = x : bubl (y:ys)




            
ordenarLista :: Ord a => [a] -> [a]
ordenarLista [] = []
ordenarLista (x:xs) = colocar x  (ordenarLista xs)


        where 
              colocar :: Ord t => t -> [t] ->[t]
              colocar x [] =[x]
              colocar x (y:ys) = if x > y then y : colocar x ys else x : y :ys 



mergeShort :: Ord a => [a] -> [a] -> [a]
mergeShort [] l = l
mergeShort l [] = l
mergeShort (x :xs) (y : ys) 
                    | x < y =     x : mergeShort xs (y:ys)
                    | otherwise = y : mergeShort (x:xs) ys

                          


