import Diccionario
import Data.Maybe
import Arbol23
import Test.HUnit

--Este módulo sirve para utilizar el diccionario sin acceder a su estructura interna. Pueden agregar otras funciones o casos de prueba.

{- Función a implementar. -}

búsquedaDelTesoro::Eq a=>a->(a->Bool)->Diccionario a a->Maybe a 
búsquedaDelTesoro pista esTesoro d = (head (filter (esTesoroONothing esTesoro) (iterate (obtenerONothing d) (Just pista))))

esTesoroONothing::(a->Bool)->Maybe a->Bool
esTesoroONothing esTesoro Nothing = True
esTesoroONothing esTesoro (Just posibleTesoro) = esTesoro posibleTesoro

obtenerONothing::Eq a=>Diccionario a a -> Maybe a -> Maybe a
obtenerONothing d c = case c of
 	Nothing -> Nothing
 	Just c -> obtener c d

{- Arbol de prueba: -}

arbolDeg::Arbol23 Char Int
arbolDeg = Dos 1 (Hoja 'a') (Dos 2 (Hoja 'b') (Tres 3 4 (Hoja 'c') (Hoja 'd') (Dos 5 (Hoja 'e') (Hoja 'f'))))
arbolOps::Arbol23 Int (Int->Int->Int)
arbolOps = Tres (+) (*) (Dos (-) (Hoja 10) (Hoja 20)) (Dos (-) (Hoja 10) (Hoja 30)) (Tres (*) (*) (Hoja 2) (Hoja 2) (Hoja 2))
arbolOps2::Arbol23 Int (Int->Int->Int)
arbolOps2 = Tres (+) (*) (Dos (+) (Hoja 10) (Hoja 20)) (Dos (+) (Hoja 10) (Hoja 30)) (Tres (*) (*) (Hoja 2) (Hoja 2) (Hoja 2))
 
{- Diccionarios de prueba: -}

dicc1::Diccionario Int String
dicc1 = definirVarias [(0,"Hola"),(-10,"Chau"),(15,"Felicidades"),(2,"etc."),(9,"a")] (vacio (<))

dicc2::Diccionario String String
dicc2 = definirVarias [("inicio","casa"),("auto","flores"),("calle","auto"),("casa","escalera"),("ropero","alfajor"),("escalera","ropero")] (vacio (<))

dicc3::Diccionario Int String
dicc3 = definirVarias [(0,"Hola"),(-10,"Chau"),(15,"Felicidades"),(2,"etc."),(9,"a")] (vacio (\x y->x `mod` 5 < y `mod` 5))

diccOrdenado::Diccionario Int Int
diccOrdenado = definirVarias [(0,1),(1,2),(2,3),(3,4),(4,5)] (vacio (<))

diccVacio::Diccionario String String
diccVacio = definirVarias [] (vacio (<))

diccA1::Diccionario Int Int
diccA1 = definir 0 0 (vacio (<))
diccA2::Diccionario Int Int
diccA2 = definir 1 0 (diccA1)
diccA3::Diccionario Int Int
diccA3 = definir 2 0 (diccA2)
diccB1::Diccionario Int Int
diccB1 = definir 2 0 (vacio (<))
diccB2::Diccionario Int Int
diccB2 = definir 1 0 (diccB1)
diccB3::Diccionario Int Int
diccB3 = definir 0 0 (diccB2)

--Ejecución de los tests
main :: IO Counts
main = do runTestTT allTests

allTests = test [
  "ejercicio2" ~: testsEj2,
  "ejercicio3" ~: testsEj3,
  "ejercicio4" ~: testsEj4,
  "ejercicio5" ~: testsEj5,
  "ejercicio6" ~: testsEj6,
  "ejercicio7" ~: testsEj7,
  "ejercicio8" ~: testsEj8,
  "ejercicio9" ~: testsEj9,
  "ejercicio10" ~: testsEj10
  ]

testsEj2 = test [
  [0,1,2,3,4,5,6,7] ~=? internos arbolito1,
  "abcdefghi" ~=? hojas arbolito1,
  [True,False,True] ~=? internos arbolito2,
  [1,2,3,2,3,4,3,4,5,4] ~=? take 10 (hojas arbolito3),
  [5,2,0,1,12,(-3),4,9,20,7] ~=? hojas (arbolito4),
  ['p','l','g','r','a','p','n','d','e'] ~=? internos (arbolito4),
  [True, False, True] ~=? internos (arbolito2),
  True ~=? esHoja (Hoja 'a'),
  False ~=? esHoja (Dos 3 (Hoja 'a') (Hoja 'a')),
  False ~=? esHoja (Tres 3 5 (Hoja 'a') (Hoja 'a') (Hoja 'a')),
  ['a','b','c','d','e','f'] ~=? hojas (arbolDeg),
  [1,2,3,4,5] ~=? internos (arbolDeg)
  ]

testsEj3 = test [
  [0,1,-1,5] ~=? hojas (incrementarHojas arbolito2),
  [-4,0,-8,16] ~=? hojas (mapA23 (\x-> x*4) id arbolito2),
  [False,True,False] ~=? internos (mapA23 id (\x->not x) arbolito2),
  ['w','w','w','w','w','w'] ~=? hojas (mapA23 (\x-> 'w') id arbolDeg),
  [0,0,0,0,0] ~=? internos (mapA23 id (\x-> 0) arbolDeg)
  ]

testsEj4 = test [
  [1,2,3,2,3,4,3,4,5,4,5,6,0,0,0,0,0] ~=? hojas (truncar 0 6 arbolito3),
  [0,0,0,0,0,0,0,0,0,0] ~=? hojas (truncar 0 3 arbolito4),
  [1,1,1,1] ~=? hojas (truncar 1 2 arbolito4),
  ['w','w','w','w','w','w','w'] ~=? hojas (truncar 'w' 2 arbolito1),
  ['w'] ~=? hojas (truncar 'w' 0 arbolito1),
  [99] ~=? hojas (truncar 99 0 arbolito4),
  [99,99] ~=? hojas (truncar 99 1 arbolito4),
  ['p'] ~=? internos (truncar 99 1 arbolito4),
  [] ~=? internos (truncar 99 0 arbolito4),
  [1] ~=? internos (truncar 'a' 1 arbolDeg),
  [1,2,3,4] ~=? internos (truncar 'a' 3 arbolDeg),
  ['a','b','a','a','a'] ~=? hojas (truncar 'a' 3 arbolDeg)
  ]

testsEj5 = test [
  45 ~=? evaluar (truncar 0 7 arbolito3),
  22 ~=? evaluar (truncar 0 6 arbolito3),
  8 ~=? evaluar (truncar 0 5 arbolito3),
  1 ~=? evaluar (truncar 0 4 arbolito3),
  (-1) ~=? evaluar (truncar 0 3 arbolito3),
  (-240) ~=? evaluar arbolOps,
  (560) ~=? evaluar arbolOps2
  ]

testsEj6 = test [
  [] ~=? claves (diccVacio)
  ]

testsEj7 = test [
  [0] ~=? claves(diccA1),
  [0,1] ~=? claves(diccA2),
  [0,1,2] ~=? claves(diccA3),
  [2] ~=? claves(diccB1),
  [1,2] ~=? claves(diccB2),
  [0,1,2] ~=? claves(diccB3),
  claves(diccA3) ~=? claves(diccB3)
  ]

testsEj8 = test [
  Nothing ~=? obtener "algo" diccVacio,
  Nothing ~=? obtener "algo" dicc2,
  Just "Felicidades" ~=? obtener 15 dicc1,
  Just "cartones" ~=? obtener "flores" (definir "flores" "cartones" dicc2)
  ]
  
testsEj9 = test [
  [] ~=? claves diccVacio,
  [0,1,2,3,4] ~=? claves diccOrdenado,
  ["auto","calle","casa","escalera","inicio","ropero"] ~=? claves dicc2,
  claves dicc1 ++ [999] ~=? claves (definir 999 "etc" dicc1)
  ]
  
testsEj10 = test [
  Just "alfajor" ~=? búsquedaDelTesoro "inicio" ((=='a').head) dicc2,
  Just 5 ~=? búsquedaDelTesoro 0 (==5) diccOrdenado,
  Nothing ~=? búsquedaDelTesoro 0 (==6) diccOrdenado,
  Just "oro" ~=? búsquedaDelTesoro "inicio" (=="oro") (definir "alfajor" "oro" dicc2)
  ]
