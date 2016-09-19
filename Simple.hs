module Simple
where

-- Definieren Sie eine Funktion fib zur Berechung der Fibonacci-Zahlen
-- ab 0 
fib     :: Integer -> Integer
fib x
		| x==0 = 0
		| x==1 = 1
		| x>=2 = fib (x-1) + fib (x-2)
--		| negative error


-- Definieren Sie eine Funktion fib zur Berechung der Fibonacci-Zahlen
-- ab 0 mit linearer Laufzeit

fib2    :: Integer -> Integer
fib2 x = fib2helper x 0 1 
			
fib2helper :: Integer -> Integer -> Integer -> Integer
fib2helper n f1 f2 
					| n==0 = f1
					| n>0 = fib2helper (n-1) f2 (f1+f2) 

-- Definieren Sie eine Funktion c (für Collatz), die berechnet
-- wie viele Rekursionsschritte benötigt werden, um
-- eine natürliche Zahl n >= 1 auf 1 zu
-- reduzieren.
--
-- Folgende Reduktionsregel sind dabei anzuwenden: Wenn n gerade ist,
-- so wird n halbiert, wenn n ungerade ist, so wird n verdreifacht und um
-- 1 erhöht.
    
c       :: Integer -> Integer
c n
	| n==1 = 0
	| even n =  c (n `div` 2) + 1
	| odd n = c ((n * 3) + 1) + 1
		


-- Definieren Sie ein endrekursive Variante von c
    
c1      :: Integer -> Integer
c1 n = c1helper n 0

c1helper :: Integer -> Integer -> Integer
c1helper n c 
			| n==1 = c
			| even n = c1helper (n `div` 2) c+1
			| odd n = c1helper ((n * 3) + 1) c+1

-- Definieren Sie eine Funktion cmax, die für ein
-- Intervall von Zahlen das Maximum der
-- Collatz-Funktion berechnet. Nutzen Sie die
-- vordefinierten Funkt min und max.

cmax    :: Integer -> Integer -> Integer
cmax lb ub 
			| lb > ub   = error "empty intervall"
			| lb == ub  = c1 lb
			| otherwise = c1 lb `max` cmax (lb + 1) ub
			


-- Definieren Sie eine Funktion imax, die für ein
-- Intervall von Zahlen das Maximum einer
-- ganzzahligen Funktion berechnet. Formulieren
-- Sie die obige Funktion cmax so um, dass sie mit imax arbeitet.

imax    :: (Integer -> Integer) -> Integer -> Integer -> Integer
imax f lb ub 
			| lb > ub   = error "empty intervall"
			| lb == ub  = f lb
			| otherwise = f lb `max` imax f (lb + 1) ub


cmax1   :: Integer -> Integer -> Integer
cmax1
    = imax c1

-- Entwickeln Sie eine Funktion,
-- die die Position und den Wert bestimmt, an der
-- das Maximum angenommen wird.
-- Versuchen Sie, eine endrekursive Lösung zu finden
-- (mit einer lokalen Hilfsfunktion).

imax2   :: (Integer -> Integer) -> Integer -> Integer -> (Integer, Integer)
imax2 f lb ub = undefined

-- ----------------------------------------
