
data BTree a = Empty
       | Node a(BTree a)(BTree a)
       deriving Show 


------ considera que a arvore é de procurar na qual a esquerda é o menor e a direita é maior


altura :: BTree a -> Int 
altura Empty = 0
altura  (Node r esd dir) =  1 +  max ( altura esd) (altura  dir)

contaNodos :: BTree a -> Int
contaNodos Empty = 0
contaNodos (Node r e d) =  1 + (contaNodos e) + (contaNodos d)


folhas :: BTree a -> Int
folhas  Empty = 0
folhas (Node r Empty Empty ) = 1
folhas (Node r e d) = (folhas e) + folhas(d) 


-------- 
prune :: Int -> BTree a -> BTree a
prune  _ Empty = Empty 
prune  n (Node r e d ) | n <= -1 = Empty
prune n (Node r e d) = Node r (prune (n-1) e) (prune (n-1) d) 


---- FALSE PARA ESQEURDA E VERDADE PARA DIREITA
path :: [Bool] -> BTree a -> [a] 
path  [] (Node r _ _) = [r]
path  _ Empty = []
path (h:t) (Node r e d)  = if h == False
                           then r : path t e else r : path t d 

mirror :: BTree a -> BTree a
mirror Empty = Empty
mirror (Node r e d)  = (Node r ( mirror d) ( mirror e))

zipWithBT :: (a -> b -> c) -> BTree a -> BTree b -> BTree c 
zipWithBT f (Node i1 e1 d1) (Node i2 e2 d2) = Node (f i1 i2) ( zipWithBT f e1 e2) ( zipWithBT f d1 d2)
zipWithBT_ _ _ = Empty


unzipBT :: BTree (a,b,c) -> (BTree a,BTree b,BTree c)
unzipBT  Empty = (Empty,Empty,Empty)
unzipBT  (Node (x,y,z) esquerda direita)  =
             let (a,b,c) = unzipBT esquerda
                 (d,e,f) = unzipBT direita

             in (Node x a d,
                 Node y b e,
                 Node z c f)             
arvore = Node (1, 'a', "um") Empty (Node (2, 'b', "dois") Empty Empty)



---2----- é uma arvore de procura na qual a esquerda é menor e direita é o maior
minimo :: Ord a => BTree a -> a
minimo (Node r Empty _ ) = r -- como a direita sempre vai ser maior entao a da direita nao importa
minimo (Node r e d) = minimo e

semMinimo :: Ord a => BTree a -> BTree a
semMinimo (Node r Empty d) = d
semMinimo (Node r e d) = Node r (semMinimo e) d


minSmin :: Ord a => BTree a -> (a,BTree a)
minSmin (Node r Empty d ) = (r, d )
minSmin (Node r e d) = (minimo e,d) 

remove :: Ord a => a -> BTree a -> BTree a
remove _ Empty = Empty
remove x (Node r e d)
    | x < r     = Node r (remove x e) d
    | x > r     = Node r e (remove x d)
    | otherwise =
        case (e, d) of
            (Empty, d') -> d'          -- só tem direita
            (e', Empty) -> e'          -- só tem esquerda
            _ ->
                let m = minimo d       -- menor da direita
                    f = semMinimo d    -- remove esse menor
                in Node m e f




removeMenor :: Ord a => BTree a -> BTree a
removeMenor Empty = Empty 
removeMenor (Node r e d) = remove (minimo (Node r e d))  (Node r e d)  


devolverSoesquerda :: Ord a=> BTree a -> BTree a
devolverSoesquerda Empty = Empty 
devolverSoesquerda (Node r e d) = remove (minimo e) e 

---fazer a funcao que trocar

taNaArvore ::Ord a => a -> BTree a -> Bool
taNaArvore _  Empty = False 
taNaArvore x (Node r e d) | x ==  r = True
                            | otherwise = if x >= r then (taNaArvore x d) else (taNaArvore x e)


nivelDaArvore :: Eq a => a -> BTree a -> Int
nivelDaArvore  _ Empty = -1
nivelDaArvore x (Node r e d) | x == r =0
                             | otherwise = let esq = nivelDaArvore x e
                                               dir = nivelDaArvore x d
                                           in    
                                                if esq /= -1
                                                    then esq + 1
                                                    else if dir /= -1
                                                        then dir + 1
                                                        else -1     
                                            

numeroNaPosicao :: Int -> BTree a ->  [a]
numeroNaPosicao n Empty =  []
numeroNaPosicao n _ | n < 0 = []
numeroNaPosicao n (Node r e d) | n == 0 = [r]
                               | otherwise = numeroNaPosicao (n-1) e ++ numeroNaPosicao (n-1) d


----- so funciona EM ARVORE BST -- na qual ordenar arvore
arvoreLista ::  BTree a -> [a]
arvoreLista Empty = []
arvoreLista (Node r e  d) = (arvoreLista e) ++ [r] ++ (arvoreLista d)


inserir :: Ord a => a -> BTree a -> BTree a
inserir n Empty = Node n Empty Empty
inserir n (Node r e d) | n == r = Node r e d 
                       | n < r = Node r (inserir n e) d
                       | otherwise = Node r e (inserir n d)


ordenardo ::  BTree a -> [a]
ordenardo Empty = []
ordenardo (Node r e d) = (ordenardo e) ++ [r] ++ (ordenardo d)

inorder ::  BTree a -> [a]
inorder Empty  = []
inorder (Node r e d) = (inorder d) ++ [r] ++ (inorder e)


{-listaArvore :: Ord a => [a] -> BTree as
listaArvore [] = Empty 
listaArvore x = Node x Empty Empty
listaArvore (x:xs:xss)  | x < xs = Node x (xs listaArvore xss) (listaArvore xss)-}

balamceamento :: BTree a -> Bool
balamceamento Empty = False
balamceamento (Node x Empty Empty) = True
balamceamento (Node x e d) | altura e == altura d = True
                           | otherwise = False 

                           




arvorePequena =
    Node 13
        (Node 10
            (Node 9 Empty Empty)
            (Node 12 Empty Empty))
        (Node 16
            Empty
            (Node 19 Empty Empty))


arvorePequena1 = Node 13 (Node 10 (Node 9 Empty Empty) (Node 12 Empty Empty)) (Node 16 Empty (Node 19 (Node 18 Empty Empty) Empty))

