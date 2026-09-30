
data ExpInt  = Const Int
 
 | Simetrico ExpInt
 | Mais ExpInt ExpInt
 | Menos ExpInt ExpInt
 | Mult ExpInt ExpInt
 deriving Show


calcula :: ExpInt -> Int 
calcula (Const x) = x
calcula (Simetrico e) = -(calcula e)
calcula(Mais e1 e2) = calcula e1 + calcula e2
calcula(Menos e1 e2) = calcula e1 - calcula e2
calcula(Mult e1 e2) = calcula e1 * calcula e2


infixa :: ExpInt -> String
infixa (Const x) = show x
infixa (Simetrico e) = "-("++(infixa e) ++ " )"
infixa (Mais e1 e2) = "(" ++ (infixa e1) ++ "+" ++(infixa e2)++ ")"
infixa (Menos e1 e2) = "(" ++ (infixa e1) ++ "-" ++(infixa e2)++ ")"
infixa (Mult e1 e2) = "(" ++ (infixa e1) ++ "*" ++(infixa e2)++ ")"

posfixa :: ExpInt -> String 
posfixa (Const x) = show x
posfixa(Simetrico e) = "-" ++ posfixa e 
posfixa(Mais e2 e3) = (infixa e2)++ (infixa e3) ++ "+"
posfixa(Menos e2 e3) = (infixa e2)++ (infixa e3) ++ "-"
posfixa(Mult e2 e3) = (infixa e2)++ (infixa e3) ++ "*"


e1 = Const 5
e2 = Mais (Const 2) (Const 3)
e3 = Mult (Const 2) (Menos (Const 10) (Const 4))
e4 = Simetrico (Const 5)



--------2. Considere o seguinte tipo para representar ´arvores irregulares (rose trees). ou arvore generalizadas 

data RTree a = R a [RTree a] --- uma arvore que nao é binario

rt :: RTree Int
rt = R 7 [ R 8 [R 1 []
             , R 13 []
             ]
        , R 2 []
        , R 9 [R 12 []
        ] ]



soma :: Num a => RTree a -> a
soma (R x l) = x + sum (map soma l)



altura :: RTree a -> Int 
altura (R x [] ) = 1 
altura (R x l) =  1 +  minimum(map altura l)




