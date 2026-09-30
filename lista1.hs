import Prelude hiding (map)

qSort :: Ord a => [a] -> [a]
qSort [] = []
qSort (h:t) =
  let (menor, maior) = partQ h t
      a = qSort menor
      b = qSort maior
  in a ++ [h] ++ b

partQ :: Ord a => a -> [a] -> ([a], [a])
partQ _ [] = ([], [])
partQ n (h:t)
  | h <= n    = (h:a, b)
  | otherwise = (a, h:b)
  where (a, b) = partQ n t
-------------------


part :: (Int -> Int) -> Int -> Int
part f x = f x

map :: (a -> b) -> [a] -> [b]
map f x = f (head x) : map f (tail x)

------------
inSort :: Ord a => [a] -> [a]
inSort [] = []
inSort (h:t) = insert h (inSort t) 


insert :: Ord a =>  a -> [a] -> [a]
insert x [] = [x]
insert x (h:t) | x <= h = x : h : t
               | otherwise = h : insert x t 