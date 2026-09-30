import GHC.Exts.Heap (GenClosure(fun))
import GHC.IOPort (doubleReadException)
import Distribution.Simple.Test.Log (TestLogs(testResult))
funA :: [Double] -> Double
funA [] = 0
funA (y:ys)= y^2 + (funA ys)

funB :: [Int] -> [Int]
funB [] = []
funB (h:t) = if(mod h 2) == 0 then h : (funB t) else (funB t)

funC (x:y:t) = funC t
funC [x] = [x]
funC [] = []

dobro :: [Int] -> [Int]
dobro [] = []
dobro (h:t) = 2*h : dobro t

numOcorre :: Char -> String -> Int
numOcorre _ [] = 0
numOcorre h l = if head l == h then 1 + numOcorre  h (tail l) else numOcorre h (tail l)

positivos :: [Int] -> Bool
positivos [x] = x >= 0  
positivos (h : t) = if h >= 0 && positivos t then True else  False 

tresUlt :: [a] -> [a]
tresUlt [] = []
tresUlt (h:t) = if length (h:t) <= 3 then (h:t) else tresUlt t

sumTriplos :: (Num a , Num b, Num c) => [(a,b,c)] -> (a,b,c)
sumTriplos [] = (0,0,0)
sumTriplos ((x,y,z):t) =  (x +x', y+y', z+z')
    where (x',y',z') = sumTriplos t


type Polinomio = [Monomio]
type Monomio  = (Float, Int)

p :: Polinomio 
p = [(2,3),(5,7),(6,4)]


conta :: Int -> Polinomio -> Int
conta  _ [] = 0
conta g (m:ms) | g == snd m = 1 + conta g ms  
               | otherwise = conta g ms

grau :: Polinomio -> Int 
grau [m] = snd m
grau ((c,e) : ms) = let g = grau ms 
                    in if e > g then e else g


ordena ::Ord a => [a] -> [a]
ordena [] = []
ordena (h:t) = insere h (ordena t)

     where 
           insere :: Ord t => t -> [t] -> [t]
           insere x [] = [x]
           insere x (y:ys) = if x <= y then x:y:ys else y : insere x ys
                