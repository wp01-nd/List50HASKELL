import Prelude hiding (concat, replicate, zip, drop, take, reverse, (!!), (++), enumFromThenTo, enumFromTo)
import Distribution.Simple.Utils (xargs)
import Control.Arrow (Arrow(first))
import Data.ByteString (isSuffixOf)
import Data.Functor.Contravariant (defaultEquivalence)




enumFromTo :: Int -> Int ->[Int]
enumFromTo x y = if x > y then [] else x : enumFromTo(x+1) y 

enumFromThenTo :: Int -> Int-> Int -> [Int]
enumFromThenTo x y z  
             | x > z =[]
             | otherwise = x : enumFromThenTo (x + (y-1)) y z 

(++) :: [a] -> [a] -> [a]
(++) [] ys = ys
(++) (x:xs) ys = x :  (xs ++ ys)

(!!) :: [a] -> Int -> a
(!!) (x : xs) n 
              | n == 0 = x
              | n > 0 = (!!) xs (n -1)

reverse :: [a] -> [a]
reverse [] = []
reverse (x:xs) = reverse xs ++ [x]

take :: Int -> [a] -> [a]
take _[] = []
take n (x:xs) | n == 0 = [x]
              | n > 0 = x : take (n -1) xs


drop :: Int -> [a] -> [a]
drop _ [] = []
drop num (x:xs) = if num == 0 then [x] else drop (num-1) xs


zip :: [a] -> [b] -> [(a,b)]
zip x [] = []
zip [] y = []
zip (xs:xss) (ys:yss) = (xs,ys) : zip xss yss

replicate :: Int -> a -> [a]
replicate 0 _ = []
replicate n l = l : replicate (n-1) l

intersperse :: a -> [a] -> [a] 
intersperse v [] = []
intersperse _ [x] = [x] 
intersperse v (x:xs) = [x] ++ [v] ++ intersperse v xs

group :: Eq a => [a] -> [[a]]
group [] = []
group (x:xs) = agrupa [x] xs

       where 
        agrupa acc [] = [acc]
        agrupa acc (y:ys)
                        | y == head acc = agrupa (acc ++ [y]) ys
                        | otherwise     = acc : group(y:ys)

concat :: [[a]] -> [a]
concat [] = []
concat (x:y:ys) = x ++ y ++ juntar  
  
  where juntar = concat ys

----13

inits :: [a] -> [[a]]
inits [] = [[]] 
inits (x: xs) = [] : adicionar x (inits xs)

  where 
       adicionar :: a -> [[a]] -> [[a]]
       adicionar  _ [] = []
       adicionar x (y:ys)  = (x:y) : adicionar x ys 


tails :: [a] -> [[a]]
tails [] = [[]]
tails (x:xs) = (x:xs) : tails (xs) 
    
heads :: [[a]] -> [a]
heads [[]] = []
heads (x:xs) = head x : heads (xs)


total :: [[a]] -> Int 
total [] = 0
total (x:xs) = length x + total xs



fun :: [(a,b,c)] -> [(a,c)]
fun [] = [] 
fun ((x,_,z) : xs) = (x,z) : fun xs 

cola :: [(String,b,c)] -> String
cola [] = ""
cola [(x,_,_)] = x
cola ((x,_,z) : (a,_,c): zx) = (x ++ a) ++ cola zx

---- mais facil
cola1 :: [(String, b, c)] -> String
cola1 [] = ""
cola1 ((x,_,_):xs) = x ++ cola xs     


---- ordem ano, idade  e ñista de nomes e idade
idade :: Int -> Int -> [(String,Int)] -> [String]
idade  a l [] = []
idade a l ((x,y):xs) = let  
                     anoPassado = a-l 

                     in 
                        if anoPassado >= y 
                            then x : idade a l xs 
                            else idade a l xs


powerEnumFrom :: Int -> Int -> [Int]
powerEnumFrom _ 0 = []
powerEnumFrom n m = powerEnumFrom n (m-1) ++ [(n^(m-1))]






isPrime :: Int -> Bool
isPrime n | n < 2 = False
          | otherwise = isPrimeFrom 2 n


 where 
    isPrimeFrom :: Int -> Int -> Bool
    isPrimeFrom m k
      | m*m > k        = True
      | k `mod` m == 0 = False
      | otherwise      = isPrimeFrom (m+1) k
-- ...existing code.

isPrefixOf :: Eq a => [a] -> [a] -> Bool
isPrefixOf [] _ = True
isPrefixOf _ [] = False
isPrefixOf (x:xs) (y:ys)
  | x == y    = isPrefixOf xs ys
  | otherwise = False 


isSubsequenceOf :: Eq a => [a] -> [a] -> Bool
isSubsequenceOf [] _ = True
isSubsequenceOf _ [] = False 
isSubsequenceOf (x:xs)(y:ys) | x == y = isSubsequenceOf xs ys 
                             | otherwise = isSubsequenceOf   (x:xs) ys                 



elemIndices :: Eq a => a -> [a] -> [Int]
elemIndices n xs = comparar 0 xs 

   where 
       comparar _ []= []
       comparar i (y:ys) | n == y = i : comparar (i+ 1) ys
                         | otherwise = comparar (i+1) ys 
                     

---26

nub :: Eq a => [a] -> [a]
nub []                  = []
nub (x:xs)
  | elem x xs         = nub xs          -- já aparece depois → ignora
  | otherwise           = x : nub xs      -- primeira ocorrência → mantém




delete :: Eq a => a -> [a] -> [a]
delete  _ [] = []
delete x (y:ys) = if x == y 
                        then ys 
                         else y : delete x ys 



(\\):: Eq a => [a] -> [a] -> [a]
(\\) [] _ = []
(\\) l [] = l
(\\) (x:xs) ys 
             | elem x ys = (\\)  xs  (delete x ys)
             | otherwise =  x : (\\) xs ys 


union :: Eq a => [a] -> [a] -> [a]
union [] l = l
union ls [] = ls
union xs (y:ys) | y `elem` xs =  union xs ys           -- y já existe → ignora
                    | otherwise   = union (xs ++ [y]) ys


intersect :: Eq a => [a] -> [a] -> [a]
intersect [] _ = []
intersect l [] = l 
intersect (x:xs)(y:ys) | x == y = x : intersect (x:xs) (delete x ys)
                       | otherwise = intersect xs (y:ys)
