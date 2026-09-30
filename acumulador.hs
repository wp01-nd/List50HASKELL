import Data.Char (isDigit, isAlpha)


digitAlpha :: String -> (String,String) 
digitAlpha [] = ([],[])
digitAlpha (h:t) | isAlpha h = ( h: letr, dig)
                 | isDigit h = (letr, h : dig)
                 | otherwise = (letr, dig)

        where (letr,dig) = digitAlpha t 


mzp :: [Int] -> (Int,Int,Int)
mzp [] = (0,0,0)
mzp (h:t) | h > 0 = (1+ pos,zero,negativo)
          | h == 0 = (pos, 1 + zero, negativo) 
          | h < 0 = (pos,zero,1+negativo)


     where(pos,zero,negativo) = mzp t



divi2 :: Integral a => a -> a -> (a,a)
divi2 m n = divmods m n (0,0)


  where divmods mn ( d, x)
         | m < n = (d, m)
         |otherwise = divmods (m-n) n ( d+1,r)
           
