--Ejercicio 1
--1
longitud ::(Num t) => [t] -> t
longitud [] = 0
longitud (x:xs) = 1 + longitud (xs)

--2
ultimo :: (Eq t) => [t] -> t
ultimo (x:xs) | xs == [] = x
              | otherwise = ultimo xs

--3
principio :: [t] -> [t]
principio [x] = []
principio (x:xs) = x : principio xs

--4
reverso :: [t] -> [t]
reverso [] = []
reverso [x] = [x]
reverso (x:xs) = reverso xs ++ [x]

--Ejercicio 2
--1
pertenece :: (Eq t) => t -> [t] -> Bool
pertenece n [] = False
pertenece n (x:xs) | n == x || pertenece n xs = True  
                  
--2
todosIguales :: (Eq t) => [t] -> Bool
todosIguales [] = False
todosIguales [x] = True
todosIguales (x:xs) | x== head xs && todosIguales xs = True
                    | otherwise = False 
       

--3
todosDistintos :: (Eq t) => [t] -> Bool {-cuando escribo [1,2,1] me dice que es true, tendria que decir false-}
todosDistintos [] = False
todosDistintos [x] = True
todosDistintos (x:xs) | x == head xs = False
                      | otherwise = todosDistintos (tail (xs))


--4
hayRepetidos :: (Eq t) => [t] -> Bool
hayRepetidos []  = False
hayRepetidos [x] = False
hayRepetidos (x:xs) = esMiembro x xs || hayRepetidos xs

esMiembro :: (Eq t) => t -> [t] -> Bool
esMiembro n [] = False
esMiembro n (x:xs) = n==x || esMiembro n xs

--5
quitar :: (Eq t) => t -> [t] -> [t]
quitar x [] = []
quitar x xs | x == head xs = tail(xs)
            | otherwise = [head xs] ++ quitar x (tail (xs))

--6
quitarTodos :: (Eq t) => t -> [t] -> [t]
quitarTodos x [] = []
quitarTodos x xs | x == head xs = [] ++ quitarTodos x (tail(xs)) 
                 | otherwise = [head xs] ++ quitarTodos x (tail(xs))

--7
eliminarRepetidos :: (Eq t) => [t] -> [t]
eliminarRepetidos (x:xs) | xs == [] = []
                         | x == head xs = xs
                         | otherwise = [x] ++ eliminarRepetidos xs

--8
mismosElementos :: (Eq t) => [t] -> [t] -> Bool --MAL, VOLVER A HACER
mismosElementos (x:[]) (y:[]) = x==y 
mismosElementos (x:xs:z) (y:ys:l) | pertenece x (y:ys:l) && pertenece y (x:xs:z) = mismosElementos (xs:z) (ys:l) 
                                  | otherwise = False

--9
capicua :: (Eq t) => [t] -> Bool
capicua (x:xs) | (x:xs) == reverso (x:xs) = True
               | otherwise = False

--Ejercicio 3
--1 
sumatoria :: [Integer] -> Integer
sumatoria (x:xs) | (x:xs) == [] = 0
                 |otherwise =  x + head (xs) + sumatoria (tail (xs))

--2 
productoria :: [Integer] -> Integer
productoria [] = 1
productoria (x:xs) = x * productoria xs

--3
maximo :: [Integer] -> Integer
maximo [x] = x
maximo (x:xs) | x>head(xs) && x> maximo(tail(xs)) = x
              | otherwise = maximo xs

--4
sumarN :: Integer -> [Integer] -> [Integer]
sumarN n [] = []
sumarN n (x:xs) | otherwise = [n+x] ++ sumarN n xs

--5
sumarElPrimero :: [Integer] -> [Integer]
sumarElPrimero (x:xs) = sumarN x (x:xs)

--6 
sumarElUltimo :: [Integer] -> [Integer]
sumarElUltimo (x:xs) = sumarN (head (reverso xs)) (x:xs)

--7
pares :: [Integer] -> [Integer]
pares [] = []
pares (x:xs) | (mod x 2 == 0) = [x] ++ pares xs
             | otherwise = pares xs

--8
multiplosDeN :: Integer -> [Integer] -> [Integer]
multiplosDeN n [] = []
multiplosDeN n (x:xs) | n == 0 = multiplosDeN n xs
                      | (mod x n == 0) = [x] ++ multiplosDeN n xs
                      | otherwise = multiplosDeN n xs

--9
ordenar :: [Integer] -> [Integer]
ordenar [] = []
ordenar (x:xs) = [minimo (x:xs)] ++ ordenar (quitar (minimo (x:xs)) (x:xs))

minimo :: [Integer] -> Integer
minimo [x] = x
minimo (x:xs) | x < head xs = minimo (x:(tail xs))
              | otherwise = minimo ((head xs):(tail xs))

--Ejercicio 4
--1
sacarBlancosRepetidos :: [Char] -> [Char]
sacarBlancosRepetidos [] = []
sacarBlancosRepetidos (x:[]) = [x]
sacarBlancosRepetidos (x:y:xs) | x==y && x==' ' = sacarBlancosRepetidos (y:xs)
                               | otherwise = [x] ++ (sacarBlancosRepetidos (y:xs))

--2
contarPalabras :: [Char] -> Integer
contarPalabras [] = 0
contarPalabras (x:xs) | x==' ' = 1 + contarPalabras xs
                      | otherwise = contarPalabras xs
                      

--3
{-palabras :: [Char] -> [[Char]]
palabras [] = [[]]
palabras (x:xs) | x == [x] ++ palabras xs
                | otherwise = palabras xs
-}

aplanar:: [[Char]] -> [Char]
aplanar[]=[]
aplanar (a:xd) | a== [' '] && head xd == [' '] = aplanar xd
               |otherwise= a ++ aplanar xd

--Ejercicio 5
--1
sumaAcumulada :: (Num t) => [t] -> [t]
sumaAcumulada [] = []
sumaAcumulada (x:y:xs) = [x] ++ sumaAcumuladaAux (x:y:xs)

sumaAcumuladaAux :: (Num t) => [t] -> [t]
sumaAcumuladaAux [x] = []
sumaAcumuladaAux (x:y:xs) = [x+y] ++ sumaAcumuladaAux ((x+y):xs)

--2
descomponerEnPrimos :: [Integer] -> [[Integer]] 
descomponerEnPrimos [x] = [descomponerEnPrimosAux x 2]
descomponerEnPrimos (x:y:xs) | x ==2 = [[2]] ++ [descomponerEnPrimosAux (head(y:xs)) 2]
                             | otherwise = [descomponerEnPrimosAux x 2] ++ [descomponerEnPrimosAux (head(y:xs)) 2]
                    

descomponerEnPrimosAux :: Integer -> Integer -> [Integer]
descomponerEnPrimosAux x t | mod x t == 0 && (esPrimo (div x t)) = [t, div x t] 
                           | otherwise = descomponerEnPrimosAux x (t+1)

esPrimo :: Integer -> Bool
esPrimo y | y==2 = True
          | mod y 2 /= 0 = True     
          | otherwise = False                        

