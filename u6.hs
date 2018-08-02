--Aufgabe 1

{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

data Uhrzeit = Lokalzeit {stunde::Int, minute::Int} 
             | LokalzeitAM {stunde :: Int, minute :: Int, am :: Bool}
             | Globalzeit {stunde :: Int, minute :: Int, verschiebung :: Int}
             deriving Show

umwandeln :: Uhrzeit -> Uhrzeit
umwandeln (Lokalzeit h m) = Lokalzeit h m
umwandeln (LokalzeitAM h m w)
    | w = (Lokalzeit h m)
    | otherwise = (Lokalzeit (h+12) m)
umwandeln (Globalzeit h m p) = (Lokalzeit (h+p) m)

instance Eq Uhrzeit where
    (==) (Lokalzeit h1 m1) (Lokalzeit h2 m2) = h1==h2 && m1==m2
    (==) w q = (==) (umwandeln w) (umwandeln q)  
instance Ord Uhrzeit where 
    (<=) (Lokalzeit h1 m1) (Lokalzeit h2 m2) = (<=) (60*h1+m1) (60*h2+m2) 
    (<=) w q = (<=) (umwandeln w) (umwandeln q) 

sort :: [Uhrzeit] -> [Uhrzeit]
sort [] = []
sort (x:xs) = (sort [z | z <-xs, z>x]) ++ [x] ++ (sort [z | z <-xs, z<=x])


--Aufgabe 2
type Vector = (Integer, Integer, Integer)
vector :: Integer -> Integer -> Integer -> Vector
vector a b c = (a, b, c)

instance Num Vector where
    (a, b, c) + (d, e, f) = ((a+d), (b+e), (c+f))
    negate (a, b, c) = ((-a), (-b), (-c))
    (a, b, c) * (d, e, f) = ((a*d), (b*e), (c*f))
    abs (a, b, c) = ((abs a), (abs b), (abs c))
    signum (a, b, c) = ((signum a), (signum b), (signum c))

--Aufgabe 3

data List a = Nil | Cons a (List a) deriving Show
infixr 5 `Cons`

konvertiere :: [a] -> List a 
konvertiere [] = Nil
konvertiere (x:xs) = x `Cons` (konvertiere xs) 
   
instance Foldable List where
    foldr f b Nil = b
    foldr f b (c `Cons` cs) = f c (foldr f b cs)

{- 
 
Aufgabe 4 Datentypen

f :: [Char] -> [[Char]] -> [Char]
Da beim Anker "" herauskommt müssen die einzelnen Elemente 
vom Typ [Char] sein, da "" :: [Char]

Wir vergleichen, demnach ist der erste Parameterr vom gleichen Typ.
Da über eine Liste gelaufen wird, deren einzelnen Elemente verglichen werden,
ist diese vom Typ [[Char]]

Die gleichen Listenelemente werden konkatiniert, also zu Typ [Char].


(Ord a) => a -> a -> a
Für den Vergleich müssen die Elemente vom gleichen Typ sein.
Sie unterliegen nur der Einschränkung, 
dass über sie die Ordnungsrelation definiert sein muss.
-}
