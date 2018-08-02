--Aufgabe 1
plusplus :: [a] -> [a] -> [a]
plusplus xs ys = foldr (:) ys xs

verbinden :: [a] -> [[a]] -> [a]
verbinden g [] = []
verbinden g [x] = x
verbinden g (x:xs) = x++g++(verbinden g xs)


entf3 :: (Eq a) => a -> [a] -> [a]
entf3 z = foldr (\ x r -> if x==z then r else x:r) []

--Aufgabe 2
--data Hour = 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 deriving Eq, Show
data Uhrzeit = Lokalzeit {stunde::Int, minute::Int} 
             | LokalzeitAM {stunde :: Int, minute :: Int, am :: Bool}
             | Globalzeit {stunde :: Int, minute :: Int, verschiebung :: Int}


gleichzeitig :: Uhrzeit -> Uhrzeit -> Bool
gleichzeitig (Lokalzeit a b) (Lokalzeit c d) = a==c && b==d
gleichzeitig (Lokalzeit a b) (LokalzeitAM c d z) 
    | z = a==c && b==d
    | otherwise = a==(c+12) && b==d
gleichzeitig (LokalzeitAM c d z) (Lokalzeit a b)
    | z = a==c && b==d
    | otherwise = a==(c+12) && b==d
gleichzeitig (LokalzeitAM a b y) (LokalzeitAM c d z) 
    | z == y = a==c && b==d
    | z = a+12==c && b==d
    | otherwise = a==c+12 && b==d
gleichzeitig (Lokalzeit a b) (Globalzeit c d e) = a==c+e && b==d
gleichzeitig (Globalzeit c d e) (Lokalzeit a b) = a==c+e && b==d
gleichzeitig (Globalzeit a b c) (Globalzeit d e f) = a+c==d+f && b==e
gleichzeitig (Globalzeit a b y) (LokalzeitAM c d z) 
    | z = a+y==c && b==d
    | otherwise = a+y==(c+12) && b==d
gleichzeitig (LokalzeitAM c d z) (Globalzeit a b y)
    | z = a+y==c && b==d
    | otherwise = a+y==(c+12) && b==d

