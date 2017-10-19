module Arbol23 where

data Arbol23 a b = Hoja a | Dos b (Arbol23 a b) (Arbol23 a b) | Tres b b (Arbol23 a b) (Arbol23 a b) (Arbol23 a b)

{- Funciones para mostrar el árbol. -}

instance (Show a, Show b) => Show (Arbol23 a b) where
    show = ("\n" ++) . (padTree 0 0 False)

padlength = 5    
    
padTree:: (Show a, Show b) => Int -> Int -> Bool -> (Arbol23 a b)-> String
padTree nivel acum doPad t = case t of 
				  (Hoja x) -> initialPad ++ stuff x
                                  (Dos x i d) -> initialPad ++ stuff x ++ 
                                                 pad padlength ++ rec x False i ++ "\n" ++
                                                 rec x True d ++ "\n"
                                  (Tres x y i m d) -> initialPad ++ stuff x ++ --(' ':tail (stuff y)) ++
                                                      pad padlength ++ rec x False i ++ "\n" ++
                                                      pad levelPad ++ stuff y ++ pad padlength ++ rec x False m ++ "\n" ++
                                                      rec x True d ++ "\n" 
  where l = length . stuff
	levelPad = (padlength*nivel + acum)
	initialPad = (if doPad then pad levelPad else "")
	rec x = padTree (nivel+1) (acum+l x)
            
stuff:: Show a => a -> String
stuff x = if n > l then pad (n-l) ++ s else s
  where s = show x
        l = length s
        n = padlength

pad:: Int -> String
pad i = replicate i ' '

{- Funciones pedidas. -}

--foldA23::
foldA23 ::(a->c)->(b->c->c->c)->(b->b->c->c->c->c)->Arbol23 a b->c
foldA23 f g h (Hoja a) = f a
foldA23 f g h (Dos b ar1 ar2) = g b (foldA23 f g h ar1) (foldA23 f g h ar2)
foldA23 f g h (Tres b c ar1 ar2 ar3) = h b c (foldA23 f g h ar1) (foldA23 f g h ar2) (foldA23 f g h ar3) 


--Lista en preorden de los internos del árbol.
internos::Arbol23 a b->[b]
internos = foldA23 (\x -> []) (\b x y -> b : x ++ y) (\b c x y z -> b : c : (x ++ y ++ z))

--Lista las hojas de izquierda a derecha.
hojas::Arbol23 a b->[a]
hojas = foldA23 (\x -> [x]) (\b x y -> x ++ y) (\b c x y z -> x ++ y ++ z)

esHoja::Arbol23 a b->Bool
esHoja ar = case ar of
		  	Hoja a -> True
		  	otherwise -> False

mapA23::(a->c)->(b->d)->Arbol23 a b->Arbol23 c d
mapA23 f g arb = foldA23 (\val -> (Hoja (f val)))
		 	 (\val ai ad ->(Dos (g val) ai ad))
		 	 (\val val2 ai ac ad ->(Tres (g val) (g val2) ai ac ad)) arb

--Ejemplo de uso de mapA23.
--Incrementa en 1 el valor de las hojas.
incrementarHojas::Num a =>Arbol23 a b->Arbol23 a b
incrementarHojas = mapA23 (+1) id

--Trunca el árbol hasta un determinado nivel. Cuando llega a 0, reemplaza el resto del árbol por una hoja con el valor indicado.
--Funciona para árboles infinitos.

truncar::a->Integer->Arbol23 a b->Arbol23 a b
truncar e lim arb = if (lim==0) then (Hoja e) else 
			foldA23 (\val ->(\i -> if(i==0) then (Hoja e) else (Hoja val) ))
		 	   	(\val ai ad ->(\i-> if (i-1==0) then (Dos val (Hoja e) (Hoja e)) else (Dos val (ai (i-1)) (ad (i-1))) ))
		 	    	(\val val2 ai ac ad ->(\i-> if (i-1==0) then (Tres val val2 (Hoja e) (Hoja e) (Hoja e)) 
									else (Tres val val2 (ai (i-1)) (ac (i-1)) (ad (i-1))) )) arb lim

--Evalúa las funciones tomando los valores de los hijos como argumentos.
--En el caso de que haya 3 hijos, asocia a izquierda.
evaluar::Arbol23 a (a->a->a)->a
evaluar a = foldA23 (\val -> val)
		    (\val ai ad -> val ai ad)
		    (\val val2 ai ac ad ->(val2 (val ai ac) ad)) a

--Ejemplo:
--evaluar (truncar 0 6 arbolito3) = 22 = (1*2-3)+(2*3-4)+(3*4-5)+(4*5-6)

{- Árboles de ejemplo. -}

arbolito1::Arbol23 Char Int
arbolito1 = Tres 0 1
        (Dos 2 (Hoja 'a') (Hoja 'b'))
        (Tres 3 4 (Hoja 'c') (Hoja 'd') (Dos 5 (Hoja 'e') (Hoja 'f')))
        (Dos 6 (Hoja 'g') (Dos 7 (Hoja 'h') (Hoja 'i')))

arbolito2::Arbol23 Int Bool
arbolito2 = Dos True (Hoja (-1)) (Tres False True (Hoja 0) (Hoja (-2)) (Hoja 4))

arbolito3::Arbol23 Int (Int->Int->Int)
arbolito3 = Dos (+) (Tres (*) (-) (Hoja 1) (Hoja 2) (Hoja 3)) (incrementarHojas arbolito3)

arbolito4::Arbol23 Int Char
arbolito4 = Dos 'p' (Dos 'l' (Dos 'g' (Hoja 5) (Hoja 2)) (Tres 'r' 'a' (Hoja 0)(Hoja 1)(Hoja 12))) 
                    (Dos 'p' (Tres 'n' 'd' (Hoja (-3))(Hoja 4)(Hoja 9)) (Dos 'e' (Hoja 20)(Hoja 7)))



