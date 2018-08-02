length' :: [a] -> Int
length' ls = sum (map (\ x -> 1) ls) 

summe_der_vielfachen :: Int -> Int -> Int
summe_der_vielfachen k n 
    | n == 0 = 0
    | mod n k == 0 = n + (summe_der_vielfachen k (n-1))
    | otherwise = summe_der_vielfachen k (n-1)

iter :: Int -> (a -> a) -> a -> a
--iter :: (Eq a, Num a) => a -> (b -> b) -> b -> b
iter 0 f x = x
iter n f x = f (iter (n-1) f x)

teiler :: Int -> [Int]
teiler n = [x | x <- [1..n], mod n x ==0]

primzahl :: Int -> Bool
primzahl n = (length' (teiler n)) == 2


--map (\ x -> div x 5) [1..20]

fr :: (a -> b -> a) -> a -> [b] -> a
fr f a [] = a 
fr f a (l:ls) = f (fr f a ls) l 



