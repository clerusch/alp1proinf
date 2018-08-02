--Aufgabe 1

ende :: [a] -> a
ende (x:[]) = x
ende (x:xs) = ende xs
ende [] = error "Keine Elemente enthalten"


abflachen :: [[a]] -> [a]
abflachen (x:[]) = x
abflachen (x:y:xs) = abflachen ((x++y):xs)


filtern :: (a -> Bool) -> [a] -> [a]
filtern p [] = []
filtern p (x:ls) 
    | p x = x:(filtern p ls)
    | otherwise = filtern p ls

filtern' :: (a -> Bool) -> [a] -> [a]
filtern' p ls = [x | x <- ls , p x]


fuer_alle :: (a -> Bool) -> [a] -> Bool
fuer_alle p [] = True
fuer_alle p (x:xs) 
    | p x = fuer_alle p xs
    | otherwise = False

--Aufgabe 2

iter :: Int -> (a -> a) -> a -> a
iter 0 f x = x
iter n f x = f (iter (n-1) f x)

g = iter 42
h = iter 0 
i = iter 21
entdecke g = g (+1) 0

--Aufgabe 3

lf :: (a -> b -> a) -> a -> [b] -> a 
lf f z [] = z
lf f z (x:xs) = lf f (f z x) xs

--Aufgabe 4
--map map :: (a->b)->[a]->[b] (c->d)->[c]->[d] 
--map map :: [c->d] -> [[c]->[d]]
--
--
--map tail :: (a->b)->[a]->[b] [c]->[c]
--map tail :: [[c]] -> [[c]]
--
--
--sum . map length :: [Int] -> Int . (a->b)->[a]->[b] [a]->Int 
--sum . map length :: [Int] -> Int . [[a]]-> [Int]
--sum . map length :: [[a]]->Int
--
--
--map length . map id :: (a->b)->[a]->[b] [c]->Int . (a->b)->[a]->[b] a->a
--map length . map id :: (a->b)->[a]->[b] [c]->Int . [a]->[a]
--map length . map id :: [[c]]->[Int] . [a]->[a]
--map length . map id :: [[c]]->[Int]
