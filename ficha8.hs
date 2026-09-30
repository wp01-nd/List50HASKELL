import Prelude hiding(max)
data Exp a = Const a
  | Simetrico (Exp a)
  | Mais (Exp a) (Exp a)
  | Menos (Exp a) (Exp a)
  | Mult (Exp a) (Exp a)
  


e = Mais(Const 3) (Mult (Const 4) (Const 5))
i = Simetrico(Const 10)

showExp :: Show a => Exp a -> String 
showExp (Const x) = show x 
showExp (Simetrico e) =  "-" ++ showExp e
showExp (Mais e1 e2) = (showExp e1) ++ "+" ++ (showExp e2)
showExp (Menos e1 e2) = (showExp e1) ++ "-" ++ (showExp e2)
showExp (Mult e1 e2) = (showExp e1) ++ "*" ++ (showExp e2)

instance Show a => Show (Exp a) 
    
    where
    show = showExp




calcular :: Num a => Exp a -> a
calcular (Const x) =  x
calcular (Simetrico e) = - (calcular e)
calcular (Mais e1 e2) = calcular e1 + calcular e2 
calcular (Menos e1 e2) = calcular e1 - calcular e2 
calcular (Mult e1 e2) = calcular e1 * calcular e2 


eqExp :: (Num a, Eq a) => Exp a -> Exp a -> Bool
eqExp e1 e2 = calcular e1 == calcular e2

instance (Num a, Eq a) => Eq (Exp a) where
    (== ) = eqExp 




-------- testar funcoes superior 

encontrar :: Eq a => a -> [a] -> Bool
encontrar x y = any (== x )  y



somar :: Int -> [Int] -> Int
somar x y  = foldr (+) x y

somarTodos :: [Int] -> [Int]
somarTodos l = map(+1) l


verSeigual :: Ord a => a -> [a] -> Bool
verSeigual x y = all (== x) y





   


 

