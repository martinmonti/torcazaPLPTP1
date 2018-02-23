import Util
import Data.Maybe
import Data.List
import Test.HUnit

-------- EJERCICIOS ----------

-- Ejercicio 1
singleton:: Eq t => t -> Anillo t
singleton e = A e (\e1 -> if (e1==e) then Just e else Nothing)

insertar :: Eq t => t -> Anillo t -> Anillo t
insertar e a = A (actual a) (proximo)
  where proximo n    | (n == (actual a)) = Just e
                    | (n == e) = (siguiente a (actual a))
                    | otherwise = (siguiente a n)

prox::(Anillo t) -> t
prox a = fromJust ((siguiente a) (actual a))

avanzar :: (Anillo t) -> (Anillo t)
avanzar a = A (prox a) (siguiente a)

-- Ejercicio 2

enAnillo:: Eq t => t -> Anillo t -> Bool
enAnillo e a = elem e $ a2L a
-- Obs: es suficiente buscar en (a2L a) porque la función a2L recorre el anillo con 
-- con la función siguiente (i.e., sólo agrega a la lista los elementos alcanzables
-- desde (actual a) mediante la función (siguiente e)).

-- Ejercicio 3
-- Dada una lista l (de elementos sin repetir), devuelve el anillo, a, que contiene los
-- mismos elementos, en el mismo orden, tal que head(l) == actual(a). Si l es vacía, 
-- entonces a == Nothing.
l2a:: Eq t => [t] -> Maybe (Anillo t)
l2a l | null l = Nothing
		| otherwise = Just $ avanzar $ foldl (\a e -> (avanzar (insertar e a))) (singleton $ head l ) (tail l)

filterAnillo :: Eq t => (t -> Bool) -> Anillo t -> Maybe (Anillo t)
filterAnillo f a = l2a $ filter f (a2L a) 

-- Ejercicio 4
-- Función que recibe una lista de a's y devuelve el anillo correspondiente de b's, donde cada b_i = f(a_i)
-- Versión que soporta Lista Vacía: 
--l2Mfa:: Eq a => Eq b => (a->b) -> [a] -> Maybe (Anillo b)
--l2Mfa f l | null l = Nothing
--			| otherwise = Just $ foldl (\a e -> (avanzar (insertar (f e) a))) (singleton $ f $ head l ) (tail l)
-- Versión que NO soporta Lista Vacía:
l2fa:: Eq a => Eq b => (a->b) -> [a] -> Anillo b
l2fa f l = avanzar $ foldl (\a e -> (avanzar (insertar (f e) a))) (singleton $ f $ head l ) (tail l)

mapAnillo:: Eq a => Eq b => (a -> b) -> Anillo a -> Anillo b
mapAnillo f a = l2fa f $ a2L a 

--Ejercicio 5
palabraFormable :: String -> [Anillo Char] -> Bool
palabraFormable palabra anillos = and([ enAnillo (palabra!!i) (anillos!!i) | i <- [0..length(palabra)-1]])

--Ejercicio 6
anillos:: Eq a => [a] -> [Anillo a]
anillos xs = [fromJust (l2a x)| x <- partes xs]

-- Dada una lista, l, sin elementos repetidos, devuelve como lista 
-- de listas el conjunto de partes del conjunto que representa l.
partes::Eq a => [a] -> [[a]]
partes l = map (\x -> destuplar l x) (combinaciones $ length $ l)

filtrarPresencias :: Eq a => [a] -> [Bool] -> [(a,Bool)]
filtrarPresencias original indicadoresDePresencia  = filter (\(a,b) -> b ) (zip original indicadoresDePresencia) -- :: [(t,Bool)]

destuplar :: Eq a => [a] -> [Bool] -> [a]
destuplar original indicadoresDePresencia = map (\(a,b) -> a ) (filtrarPresencias original indicadoresDePresencia)

-- Dado un natural, n, devuelve las 2^n listas de booleanos posibles 
-- de longitud n (es decir, todas las combinaciones)
combinaciones :: Int -> [[Bool]]
--combinaciones n =  [(combinaciones(n-1)!!i)++[b] | b<-[True,False],i<-[0..2^(n-1)]] 
--combinaciones 3 = [[True,True,True],[True,True,False],[True,False,True],[True,False,False],[False,True,True],[False,True,False],[False,False,True],[False,False,False]]
--combinaciones 2 = [[True,True],[True,False],[False,True],[False,False]]
--combinaciones 1 = [[True],[False]]
--combinaciones 0 = []
combinaciones n | n==1 = [[True],[False]]
					| otherwise =  [(combinaciones(n-1)!!i)++[b] | b<-[True,False],i<-[0..2^(n-1)-1]] 


--------   TESTS    ----------


{- Anillos de prueba: -}
--anilloEjemplo :: Anillo Integer
--anilloEjemplo = A 5 proximo where
--    proximo n   | n == 5 = Just 8
--                | n == 8 = Just 3
--                | n == 3 = Just 7
--                | n == 7 = Just 5
--                | otherwise = Nothing
anilloUnico :: Anillo Char
anilloUnico = A 'a' (\_ -> Just 'a')
anilloUnicoB :: Anillo Char
anilloUnicoB = A 'b' (\_ -> Just 'b')
anilloAsd :: Anillo Char
anilloAsd = A 'a' proximo where
    proximo n   | n == 'a' = Just 's'
                | n == 's' = Just 'd'
                | n == 'd' = Just 'a'
                | otherwise = Nothing
anilloSda :: Anillo Char
anilloSda = A 's' proximo where
    proximo n   | n == 'a' = Just 's'
                | n == 's' = Just 'd'
                | n == 'd' = Just 'a'
                | otherwise = Nothing
anilloA1 :: Anillo Char
anilloA1 = A 'l' proximo where
    proximo n   | n == 'l' = Just 'f'
                | n == 'f' = Just 'q'
                | n == 'q' = Just 'a'
                | n == 'a' = Just 'l'
                | otherwise = Nothing
anilloA2 :: Anillo Char
anilloA2 = A 'x' proximo where
    proximo n   | n == 'x' = Just 'u'
                | n == 'u' = Just 'x'
                | otherwise = Nothing
anilloA3 :: Anillo Char
anilloA3 = A 'i' proximo where
    proximo n   | n == 'i' = Just 'a'
                | n == 'a' = Just 'z'
                | n == 'z' = Just 'i'
                | otherwise = Nothing
anilloA4 :: Anillo Char
anilloA4 = A 'p' proximo where
    proximo n   | n == 'p' = Just 'n'
                | n == 'n' = Just 's'
                | n == 's' = Just 'd'
                | n == 'd' = Just 'p'
                | otherwise = Nothing
listaAnillosPF = [anilloA1, anilloA2, anilloA3, anilloA4]



--Ejecución de los tests:
main :: IO Counts
main = do runTestTT allTests
allTests = test [
--  "ejercicio1" ~: testsEj1,
  "ejercicio2" ~: testsEj2,
  "ejercicio3" ~: testsEj3, 
  "ejercicio4" ~: testsEj4,
  "ejercicio5" ~: testsEj5
--  "ejercicio6" ~: testsEj6
  ]
--testsEj1 = test [
--  anilloUnico ==  Main.singleton 'a',
--  anilloAsd ==  insertar 'd' (insertar 's' anilloUnico),
--  anilloSda ==  avanzar anilloAsd  
--  ]
testsEj2 = test [
  True ~=? enAnillo 3 anilloEjemplo,
  False ~=? enAnillo 12 anilloEjemplo,
  False ~=? enAnillo 9 anilloEjemplo,
  True ~=? enAnillo 'j' (insertar 'j' anilloAsd)
  ]
testsEj3 = test [
  True ~=? enAnillo 8 (fromJust (filterAnillo (>5) anilloEjemplo)),
  False ~=? enAnillo 3 (fromJust (filterAnillo (>5) anilloEjemplo))
  ]
testsEj4 = test [
  True ~=? enAnillo 9 (mapAnillo (+1) anilloEjemplo),
  True ~=? enAnillo 6 (mapAnillo (+1) anilloEjemplo),
  False ~=? enAnillo 5 (mapAnillo (+1) anilloEjemplo),
  Just 8 ~=? (siguiente (mapAnillo (+1) anilloEjemplo) 4 ),
  6 ~=? actual (mapAnillo (+1) anilloEjemplo),
  True ~=? enAnillo 6 (mapAnillo (*2) anilloEjemplo),
  True ~=? enAnillo 14 (mapAnillo (*2) anilloEjemplo),
  False ~=? enAnillo 5 (mapAnillo (*2) anilloEjemplo),
  Just 14 ~=? siguiente (mapAnillo (*2) anilloEjemplo) 6,
  10 ~=? actual (mapAnillo (*2) anilloEjemplo)
  ]
testsEj5 = test [
  True ~=? palabraFormable "luis" listaAnillosPF,
  False ~=? palabraFormable "flan" listaAnillosPF,
  True ~=? palabraFormable "quid" listaAnillosPF
  ]
--testsEj6 = test [
--  anillos ['a', 'b'] ~=? [anilloUnico, anilloUnicoB, insertar 'b' anilloUnico]
--  ]
