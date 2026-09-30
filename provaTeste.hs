import Distribution.Compat.Prelude (toUpper, Alternative (some), toLower)
import GHC.Base (IO(IO))
import Text.Read (Lexeme(String))



data Turma  =  Empty | Node (Integer, String) Turma Turma 
               deriving (Show)


----- ja ta ordenado

turmaToList :: Turma -> [(Integer, String)]
turmaToList Empty = []
turmaToList (Node aluno esq dir) =
    turmaToList esq ++ [aluno] ++ turmaToList dir

   

alternar :: Num a => Int -> a -> [a]
alternar 0 _ = []
alternar x y | x == 1 = [y]
             | otherwise = y : -y : alternar (x-2) y 


----- da uma maior e o menor numero da lista
limites :: Turma -> (Integer, Integer)
limites Empty = (0,0)
limites (Node alunos esq dir) = ( minimo, maximo )
   where 
    lista = turmaToList turmaTeste
    soNumero = map fst lista
    maximo = maximum soNumero
    minimo = minimum soNumero






turmaTeste :: Turma
turmaTeste =
  Node (15, "Ana")
    (Node (10, "João")
        (Node (5, "Rita") Empty Empty)
        (Node (12, "Luís") Empty Empty)
    )
    (Node (20, "Maria")
        Empty
        (Node (25, "Pedro") Empty Empty)
    )

----2

-----a
type Palavra = String
type Abreveatura = String
type TabAbre = [(Palavra,Abreveatura)]


verificar :: (Palavra, Abreveatura) -> Int
verificar (palavra, abre) = length palavra - length abre

-- Função principal
difMaior :: TabAbre -> (Palavra, Int)
difMaior [] = error "Tabela vazia"    -- ou poderia retornar ("", 0), mas error é mais seguro
difMaior [unico] = (fst unico, verificar unico)   -- caso com apenas um elemento
difMaior (x : xs) = 
    let melhorResto = difMaior xs                  -- recursão: melhor do resto da lista
        difX       = verificar x
        difMelhor  = snd melhorResto               -- diferença do melhor do resto
    in  if difX > difMelhor
        then (fst x, difX)                          -- x é melhor
        else melhorResto


tabela = [("ana", "a"), ("wu hou pan", "wu pan")]





data LTree a = Tip a | Fork ( LTree a) (LTree a)
            deriving Show




uniless :: [String] -> String
uniless [x] = x
uniless  (x:xs) = x ++ "\n" ++ uniless xs
nomes = ["ana", "fatima", "pablo"]




type Mat = [[Int]]



stringToVector :: [String] -> [[String]]
stringToVector [x] = [[x]]
stringToVector (x : xs) =  [x] : stringToVector xs


matriz = ["2 3 4", "5 6 7"]



data Lista a = Esq a (Lista a) 
              | Dir a (Lista a)
              | Nula

------ compara e pega o menor,
testarMin :: Ord a => a -> a -> a
testarMin x y = min x y


------ filter filtra todos os elementos nas certas condicoes 
comparar :: Ord a => a -> [a] ->[a]
comparar x y = filter (x ==)  y   

----- map aplicar uma fucao na lista
compararTOdas :: Ord a => a -> [[a]] -> [[a]]
compararTOdas x y = map (comparar x) y



------ estar folder 


testarFoldr :: [a] -> [[a]] -> [a] 
testarFoldr  x y = foldr (++) x y 





----- testar all

-- ...existing code...
seTiverRetorne :: Eq a => a -> [a] -> [a]
seTiverRetorne letra palavra = if verificar letra palavra then palavra else []
  where
    verificar :: Eq a => a -> [a] -> Bool
    verificar x y = any (x ==) y
-- ...existing code...


---------- funcoes anonimas 

anonima1 ::   [(Int, Int)] -> [(Int,Int)]
anonima1 l = map (\(x,y) -> (y,x) )l    ----- o que faz aplicar para toda a lista o valor anonimo


anonima2 :: [Int] -> [Int]
anonima2 l = filter (\x  -> mod x 3 == 0 ) l


anonima3 ::  Char -> String -> String
anonima3  = (\x y -> comparar x y )

anonima4 :: String -> String -> Bool
anonima4 = (\ x y -> x == y)

anonima5 :: String -> String
anonima5 [] = []
anonima5 l = (\(x: xs) -> anonima5 xs ++ [x] ) l


------uso do case of
data Arma = Lucas| Faca | Foice 

atirarArma :: Arma -> Bool
atirarArma arma = case arma of
    Lucas -> True
    _ -> False




------ 5




colocarMaiusculo :: String -> String
colocarMaiusculo [] = []
colocarMaiusculo [x] = [x]
colocarMaiusculo (x:y:xs)
  | x == '\n' = x : toUpper y : colocarMaiusculo xs
  | otherwise = toUpper x : colocarMaiusculo (y:xs)



------ uso do !! --- o que eu entendo ele acessar o indice do valor desejado 





type Matriz = [[Int]] 



retornarMatriz :: Matriz -> Int -> [Int]
retornarMatriz mat n
  | n < 0 || n >= length mat = []   -- evita erro de índice
  | otherwise   = mat !! n
-- ...existing code...
   
mat :: Matriz
mat = [[1,2,3],
       [4,5,6],
       [7,8,9]]

------ main 


main = do 
    putStrLn "Qual é o teu nome?" 
    nome <- getLine 
    putStrLn ("Olá " ++ nome ++ "! Bem-vindo ao Haskell 😊") 


dialogo :: String -> IO String
dialogo s = do putStr s
               r <- getLine
               return r

falarROBOT  :: String -> IO String
falarROBOT palavra = do  
                           putStrLn " O QUE DO DESEJA"
                           obj <- getLine
                           if palavra == obj then return palavra else return obj


somarNumerosLista :: [Int] -> IO [Int]
somarNumerosLista l = do
  putStrLn "Dá um número para acrescentar à lista:"
  s <- getLine
  let n = read s :: Int ---- precimoos converter para INt pois o getline pegar so String
  if null l 
    then return []     
    else return (map (+n) l )

aplicarFuncoesSuperior ::  [Int] -> IO [Int]
aplicarFuncoesSuperior  l = do 
        putStrLn "da um numero e ve se ta na lista"
        r  <- getLine 
        let n  = read r :: Int 
        if null l 
          then return []
          else return (filter(== n) l)

verListaNomesIguias :: [String] -> IO [String]
verListaNomesIguias l = do 
        putStrLn "digite o nome para ver se existir"
        r <- getLine
        if null l 
           then return [] else return (filter(== r) l)

compararString :: String -> String -> Bool
compararString a b = map toLower a == map toLower b

compararLetra :: String -> IO String
compararLetra l = do
  putStrLn "escreva uma palavra para ver se é igual"
  palavra <- getLine
  let p = map toLower palavra
  if null l
    then return ""              -- vazio em vez de [] para String
    else if compararString  l p
           then return l
           else return ""



-------- metir uma funcao qualquer ai :r

funcaoQual ::  Int -> [Int] -> [[Int]]
funcaoQual  _ [] = []
funcaoQual x ls = if x > 0 then  ls : funcaoQual (x-1) ls else []



infixo :: [Int] -> [[Int]]
infixo [] = [[]]
infixo (x : xs) = [] : map (x:)  (infixo xs)


sufixo :: [Int] -> [[Int]]
sufixo []= [[]]
sufixo (x:xs) = (x:xs) : sufixo xs


----- funcao da prova 

------[7 0 0 0 2] = 7 + 2x⁴ 




polinomio :: [Float] -> [Float] -> [Float]
polinomio l [] = l
polinomio [] l = l
polinomio (x:xs) (y:ys) = x+y : polinomio xs ys













-------- recurso 
----Questao 1
descomprime :: [(a,Int)] -> [a]
descomprime [] = []
descomprime ((x,n):xs) | n > 0 = x : descomprime((x,(n-1)): xs) 
                       | otherwise = descomprime xs 



----Questao 3
diferentes :: Eq a => [a] -> Bool 
diferentes [] = True
diferentes (x:xs) = if all(/= x) xs && diferentes xs then True else False 


----Questao 4
----a)
freq :: Eq a => [a] -> [(a,Int)]
freq [] = []
freq (x:xs) = (x, 1 + length n) : freq (delete x xs)

   where 
     n = filter (==x) xs

     delete  _ [] = []
     delete x (y:ys) = if x == y 
                        then delete x ys 
                         else y : delete x ys
---b)
grafico :: String -> IO ()
grafico s = aux (freq s)
  where
    aux [] = return ()
    aux ((c,n):xs) = do
      putStrLn (c : "(" ++ show n ++ ") " ++ replicate n '*')
      aux xs



data Exp a
  = Const a
  | Var String
  | Add (Exp a) (Exp a)
  | Mul (Exp a) (Exp a)

instance Show a => Show (Exp a) where
  show (Const x) = show x
  show (Var s)   = s
  show (Add e1 e2) = "(+) " ++ "(" ++ show e1 ++ ") " ++ "(" ++ show e2 ++ ")"
  show (Mul e1 e2) = "(*) " ++ "(" ++ show e1 ++ ") " ++ "(" ++ show e2 ++ ")"


eval :: Exp Int -> [(String, Int)] -> Int
eval (Const x) _ = x
eval (Var s) env = lookupVar s env
eval (Add e1 e2) env = eval e1 env + eval e2 env
eval (Mul e1 e2) env = eval e1 env * eval e2 env

lookupVar :: String -> [(String, Int)] -> Int
lookupVar _ [] = 0   -- ou erro, mas em prova costuma aceitar
lookupVar s ((x,v):xs)
  | s == x    = v
  | otherwise = lookupVar s xs



data 
   LRTRee a = Leaf a 
      | Folha [LRTRee a] deriving (Show)

-- Exemplo lrt
lrt :: LRTRee Integer
lrt = Folha[Leaf 3, Folha [Leaf 6, Folha [Leaf 7]], Leaf 8, Folha [Folha [Leaf 12], Leaf 10]]


altura :: LRTRee a -> Int
altura (Leaf _) = 1
altura (Folha xs) = 1 + maximum (map altura xs)