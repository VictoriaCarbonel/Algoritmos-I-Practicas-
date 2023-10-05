--relacionesValidas 
relacionesValidas :: [(String, String)] -> Bool
relacionesValidas [] = True
relacionesValidas [(a,b)] = a/=b 
relacionesValidas (x:y:xs) | x==y = False
                           | (fst x) == (snd y) && (snd x) == (fst y) || (fst x)==(fst y) && (snd x)==(fst y) = False
                           | otherwise = relacionesValidas (y:xs) && relacionesValidas (x:xs)

--problema personas
personas :: [(String, String)] -> [String]
personas [] = []
personas [x] = personasAux1 (((fst x), (snd x)))
personas (x:y:xs) | otherwise = borrarRepetidas ((personasAux1 (((fst x), (snd x)))) ++ personas (y:xs)) 

personasAux1 :: (String, String) -> [String]
personasAux1 (x,y) = [x,y]

borrarRepetidas :: (Eq t) => [t] -> [t]
borrarRepetidas [] = []
borrarRepetidas [x] = [x]
borrarRepetidas (x:y:xs) | (elem x (y:xs)) = borrarRepetidas (y:xs)
                         | otherwise = [x] ++ borrarRepetidas (y:xs)


--problema amigosDe 
amigosDe :: (Eq t) => t -> [(t, t)] -> [t]
amigosDe x [] = []
amigosDe x [y] = amigosDeAux x y
amigosDe x (y:z:xs) | otherwise = amigosDeAux x y ++ amigosDe x (z:xs)

amigosDeAux :: (Eq t) => t -> (t, t) -> [t]
amigosDeAux x (y,ys) | x == y = [ys]
                     | x == ys = [y]
                     | otherwise = []

--problema personaConMasAmigos 
personaConMasAmigos :: [(String, String)] -> String
personaConMasAmigos [] = []
personaConMasAmigos [x] = fst x
personaConMasAmigos (x:y:ys) | (contar x y) == (contar x (head ys)) = contar x y
                             | otherwise = personaConMasAmigos (y:ys)

contar :: (String, String) -> (String, String) -> String
contar (x,y) (n,m) | elem x (n,m) == elem y (n,m) = y
                   | elem x (n,m) > elem y (n,m) = x 
                   | otherwise = y

