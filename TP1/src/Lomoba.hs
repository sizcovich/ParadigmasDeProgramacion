module Lomoba where
import Grafo
import Tipos


-- ---------------------------------Sección 6--------- Lomoba ---------------------------

-- Ejercicio 10
foldExp ::     (Prop -> a)    -- Función para aplicar a (Var p)
			-> (a -> a)       -- Función para aplicar a (Not e)
			-> (a -> a -> a)  -- Función para aplicar a (Or e1 e2)
			-> (a -> a -> a)  -- Función para aplicar a (And e1 e2)
			-> (a-> a) 		  -- Función para aplicar a (D e)
			-> (a -> a) 	  -- Función para aplicar a (B e)
			-> Exp -> a       -- Función que construye el fold
foldExp fVar fNot fOr fAnd fd fb (Var p) = fVar p
foldExp fVar fNot fOr fAnd fd fb (Not e) = fNot (foldExp fVar fNot fOr fAnd fd fb e) 
foldExp fVar fNot fOr fAnd fd fb (Or a b) = fOr (foldExp fVar fNot fOr fAnd fd fb a) (foldExp fVar fNot fOr fAnd fd fb b)
foldExp fVar fNot fOr fAnd fd fb (And a b) = fAnd (foldExp fVar fNot fOr fAnd fd fb a) (foldExp fVar fNot fOr fAnd fd fb b)
foldExp fVar fNot fOr fAnd fd fb (D e) = fd (foldExp fVar fNot fOr fAnd fd fb e)
foldExp fVar fNot fOr fAnd fd fb (B e) = fb (foldExp fVar fNot fOr fAnd fd fb e)

-- Ejercicio 11
visibilidad :: Exp -> Integer
visibilidad = undefined

-- Ejercicio 12
extraer :: Exp -> [Prop]
extraer = undefined

-- Ejercicio 13
eval :: Modelo -> Mundo -> Exp -> Bool
eval mod w exp = (eval' mod exp) w

-- Dado un modelo y una expresión, devuelve una función
-- que para un Mundo dado (en el modelo), devuelve la evaluación.
-- En los pasos de D y B, se aplica la función recursiva sobre todos
-- los mundos vecinos y se busca que en alguno o en todos la expresión
-- sea true.
eval' :: Modelo -> Exp -> (Mundo -> Bool)
eval' (K g mundosTrue) =
	foldExp 
		(\p w -> w `elem` (mundosTrue p)) -- ::Prop -> (Mundo -> Bool)
		(\rec w -> not (rec w)) -- ::(Mundo -> Bool) -> Mundo -> Bool
		(\rec1 rec2 w -> (rec1 w) || (rec2 w))
		(\rec1 rec2 w -> (rec1 w) && (rec2 w))
		(\rec w -> or (map rec (vecinos g w))) -- rec::(Mundo -> Bool)
		(\rec w -> and (map rec (vecinos g w)))



-- Ejercicio 14
valeEn :: Exp -> Modelo -> [Mundo]
valeEn = undefined

-- Ejercicio 15
-- Usando foldr, voy construyendo un nuevo modelo de Kriptke, partiendo
-- del modelo pasado por argumento y sacándole los mundos donde no vale
-- la expresión. Por cada mundo que saco, le saco el nodo al grafo y
-- lo saco del valor de retorno de la función (para cada símbolo
-- proposicional).
quitar :: Exp -> Modelo -> Modelo
quitar e mod@(K g mundosTrue) =
			foldr (\w (K gRec fRec) -> 
							K (sacarNodo w gRec)
							  (\prop -> filter (/=w) (fRec prop))
				  )
				  mod                  -- Si e vale en todos los mundos, devuelvo el modelo original
				  (valeEn (Not e) mod) -- mundos donde NO vale e

-- Ejercicio 16
cierto :: Modelo -> Exp -> Bool
cierto = undefined

