--Ejercicio 1
fibonacci :: Integer -> Integer
fibonacci n | n==0 = 0
            | n==1 = 1
            | otherwise = fibonacci(n-1) + fibonacci(n-2)
             
--Ejercicio 2
parteEntera :: Float -> Integer
parteEntera x | x>=0 && x<1 = 0                                      
              | x>(-1) && x<=0 = -1 
              | x>=1 = 1 + parteEntera(x-1)
              | otherwise = (-1) + parteEntera(x-1)


--Ejercicio 4
sumaImpares :: Integer -> Integer
sumaImpares y = sumaAux (impares 1 y)

impares :: Integer -> Integer -> [Integer]
impares x y | x==y = 

sumaAux :: [Integer] -> Integer
sumaAux [x] = x
sumaAux (x:xs) = x+ head(xs) + sumaAux (tail (xs)) 

--Ejercicio 5
medioFact :: Integer -> Integer
medioFact x | x==0 = 0
            | x==1 = 1
            | x==2 = 2
            | otherwise = x * medioFact(x-2) 

--Ejercicio 6
sumaDigitos :: Integer -> Integer
sumaDigitos x | x<10 = x
              | otherwise = sumaDigitos(mod x 10) + sumaDigitos(div x 10)

--Ejercicio 7
todosDigitosIguales :: Integer -> Bool
todosDigitosIguales x | x<10 = True
                      | otherwise = ((mod x 10)==(mod (div x 10) 10)) && todosDigitosIguales(div x 10)

--Ejercicio 8
iesimoDigito :: Integer -> Integer -> Integer
iesimoDigito x i | x == 0 = 0
                 | otherwise = mod (div x (10^((cantDigitos x) - i))) 10 

cantDigitos :: Integer -> Integer
cantDigitos x | x==0 = 0
              | otherwise = 1 + cantDigitos(div x 10)

--Ejercicio 9
esCapicua :: Integer -> Bool 
esCapicua n | n<10 = True
            | otherwise = ((iesimoDigito n 1) == (ultimoTermino n)) && ((iesimoDigito n (cantDigitos n-1)) == (mod (div n 10) 10))

ultimoTermino :: Integer -> Integer
ultimoTermino x = mod x 10

--Ejercicio 10
ejercicioA :: Integer -> Integer 
ejercicioA x | x == 0 = 1
             | otherwise = 2^x + ejercicioA(x-1)

ejercicioB :: Integer -> Integer -> Integer
ejercicioB q n | q==1 = n
               | otherwise = (q^n) + ejercicioB (n-1) q

ejercicioC :: Integer -> Integer -> Integer
ejercicioC q n | q==1 = (q^2) + 1
               | otherwise = ((q^2)^n) + (ejercicioC q (n-1)) {- Cuando la ejecuto la terminal no responde-}

ejercicioD :: Integer -> Integer -> Integer
ejercicioD n q = (q^(2*n)) + ejercicioB (n-1) q

--Ejercicio 11
eAprox :: Integer -> Float
eAprox x | x == 0 = 1
         | otherwise = (1 / factorial x) + eAprox (x-1) 

factorial :: Integer -> Float
factorial x | x == 0 = 1.0
            | otherwise = (fromIntegral x) * (factorial(x-1)) 

--Ejercicio 12
raizDe2Aprox :: Integer -> Float
raizDe2Aprox n | n==1 = 2
               | otherwise = 2 + (1 / raizDe2Aprox (n-1))

--Ejercicio 13
dobleSumatoria :: Integer -> Integer -> Integer
dobleSumatoria x y | x == 0 = 0 
                   | otherwise = sumatoriaInterna (x-1) y + sumatoriaInterna x y 

sumatoriaInterna:: Integer -> Integer -> Integer
sumatoriaInterna x y | y == 0 = 0
                     | otherwise = x^y + sumatoriaInterna x (y-1) 

{--Ejercicio 14
sumaPotencias :: Integer -> Integer -> Integer -> Integer
sumaPotencias q n m | n==0 = 0 
                    | otherwise = q^(n+m) + sumaPotencias q (n-1) m + -}

--Ejercicio 15
sumaRacionales :: Integer -> Integer -> Float
sumaRacionales p q | p == 0 = 0 
                   |otherwise = racionalInterno p q + racionalInterno (p-1) q

racionalInterno :: Integer -> Integer -> Float
racionalInterno p q | q == 1 = fromIntegral p
                    | otherwise = fromIntegral p/ fromIntegral q + racionalInterno p (q-1)

--Ejercicio 16
--a
menorDivisor :: Integer -> Integer
menorDivisor x | x == 1 = 1
               | otherwise = masChico x 2

masChico :: Integer -> Integer -> Integer
masChico x y | mod x y == 0 = y
             | otherwise = masChico x (y+1)

--b
esPrimo :: Integer -> Bool
esPrimo x | x==2 = True
          | otherwise = calculoPrimo x 2

calculoPrimo :: Integer -> Integer -> Bool
calculoPrimo x y | x==y = True
                 | mod x y /= 0 && calculoPrimo x (y+1) = True
                 | otherwise = False

--c
sonCoprimos :: Integer -> Integer -> Bool
sonCoprimos x y | x==y = True
                | otherwise = calculoCoprimos x y 2

calculoCoprimos :: Integer -> Integer -> Integer -> Bool
calculoCoprimos x y n | x == y = True
                      | (mod x n == 0) == (mod y n == 0) || calculoCoprimos x y (n+1) = True
                      | otherwise = False


--d
nEsimoPrimo :: Integer -> Integer
nEsimoPrimo x | x == 1 = 2
              | otherwise = calculonEsimo (nEsimoPrimo (x-1))       

calculonEsimo :: Integer -> Integer
calculonEsimo x | esPrimo (x+1) = x+1
                | otherwise = calculonEsimo (x+1)


--Ejercicio 17
esFibonacci :: Integer -> Bool
esFibonacci n = fibonacciBool n 0
              
fibonacciBool :: Integer -> Integer -> Bool
fibonacciBool n x | n==fibonacci x = True
                  | fibonacci x > n = False
                  | otherwise = fibonacciBool n (x+1)

--Ejercicio 18
mayorDigitoPar :: Integer -> Integer
mayorDigitoPar n | n==0 = -1
--                 | 
--                 | otherwise = -1
{-
medioDigito:: Integer -> Integer
medioDigito x = ultimoDigito (primerDigito x)

sacarUltimo :: Integer -> Integer
sacarUltimo x = div x 10
-}
digito :: Integer -> Integer
digito x = mod x 10

esPar :: Integer -> Bool
esPar x = mod x 2 == 0 
                



--Ejercicio 19
--esSumaIncialDePrimos :: Int -> Bool
--esSumaIncialDePrimos x = primosHasta x 2             

{- primosHasta :: Int -> Int -> Int
primosHasta x n | x==n = x 
                | x<n = n
                | n==2 = 2 + primosHasta x (n+1)
                | (mod n 2 /= 0) =  n + primosHasta x (n+1) -}
            

             
