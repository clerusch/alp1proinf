--Aufgabe 1

ist_dreieck :: Double -> Double -> Double -> Bool
ist_dreieck a b c = (c <= a+b) && (b <= a+c) && (a <= b+c)

drei_trip :: [(Double, Double, Double)]
drei_trip = [(a,b,c) | a <- [1,6,20],b <- [1,6,20],c <- [1,6,20] , ist_dreieck a b c]

lendreitrip :: Int
lendreitrip = length(drei_trip)

--Die erzeugte Liste hat 12 Elemente

--Aufgabe 2
zinsen :: Double -> Double -> Double
zinsen kapital zinsfuß = kapital * zinsfuß * 0.01

endwert :: Double -> Double -> Double
endwert kapital zinsfuß = kapital + (zinsen kapital zinsfuß)

ew :: Double -> Double -> Int -> Double
ew kap fu 1 = kap + (zinsen kap fu)
ew kap fu n = ew (kap + (zinsen kap fu)) fu (n-1)

endwert2 :: Double -> Double -> Double
endwert2 kap fu = ew kap fu 2


--Aufgabe 3
flaeche :: Double -> Double -> Double -> Double
flaeche a b c = sqrt(s*(s-a)*(s-b)*(s-c)) where s = (a+b+c)/2


flaeche2 :: Double -> Double -> Double -> Double
flaeche2 a b c = let s = (a+b+c)/2 in sqrt(s*(s-a)*(s-b)*(s-c))


flaeche3 :: Double -> Double -> Double -> Double
flaeche3 a b c = if (ist_dreieck a b c)
    then flaeche a b c 
    else (-1)


--Aufgabe 4
type Datum = (Int, Int, Int)

datumzutagen :: Datum -> Int
datumzutagen (j, m, t) = 365 * (j-1) + (monat (m-1)) + t

monat :: Int -> Int
monat 0 = 0
monat 2 = 28 + 31
monat n = if n <= 7 && (mod n 2 == 1) || n > 7 && (mod n 2 == 0)
    then 31 + (monat (n-1))
    else 30 + (monat (n-1))


tagesdifferenz :: Datum -> Datum -> Int
tagesdifferenz (j, m, t) (y, n, d) = (max (datumzutagen (j,m,t)) (datumzutagen (y,n,d))) - (min (datumzutagen (j,m,t)) (datumzutagen (y,n,d)))

-- Rückgabewert ist jeweils Int, da die Tagesdifferenz gefragt ist. 
-- Da Tage des weiteren innerhalb des Datum-Tupels bereits als Ints definiert sind, ist dies eine logische Schlussfolgerung.
