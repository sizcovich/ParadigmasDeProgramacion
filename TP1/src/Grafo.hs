module Grafo (Grafo, vacio, nodos, vecinos, agNodo, sacarNodo, agEje, lineal, union, clausura) where

data Grafo a = G [a] (a -> [a])

instance (Show a) => Show (Grafo a) where
	show (G n e) = "[\n" ++ concat (map (\x -> " " ++ show x ++ " -> " ++ show (e x) ++ "\n") n) ++ "]"


-- ---------------------------------Sección 3--------- Grafos ---------------------------

-- Ejercicio 1
-- Crea un nuevo grafo con una lista vacía de nodos y una función que
-- devuelve siempre "undefined" (porque no hay nodos, y el dominio de
-- la función de ejes es el conjunto de nodos).
vacio :: Grafo a
vacio = G [] (const undefined)

-- Ejercicio 2
nodos :: Grafo a -> [a]
nodos = undefined

-- Ejercicio 3
vecinos :: Grafo a -> a -> [a]
vecinos = undefined

-- Ejercicio 4
agNodo :: a -> Grafo a -> Grafo a
agNodo = undefined

-- Ejercicio 5
-- Construye un nuevo grafo:
--    * Filtra la lista de nodos para sacar el nodo.
--    * Crea una nueva función que se indefine para el nodo que acabamos
--      de sacar y para los demás nodos devuelve los mismos vecinos de
--      antes salvo el nodo que se sacó.
sacarNodo :: a -> Grafo a -> Grafo a
sacarNodo n (G nodos ejes) = G (filter (/=n) nodos) (\x -> if (x==n) then undefined else (filter (/=n) (ejes x)))

-- Ejercicio 6
agEje :: (a,a) -> Grafo a -> Grafo a
agEje = undefined

-- Ejercicio 7
-- Si la lista es vacía, crea un grafo vacío.
-- Si la lista no es vacía, recursivamente va agregando nodos al grafo,
-- y a partir del segundo nodo que agrega, también va agregando ejes.
-- Es por eso que en el paso inductivo primero pregunta si el grafo
-- "rec" ya tiene algún nodo y si es así agrega un eje entre el último
-- nodo agregado y el que se está agregando en este paso.
lineal :: [a] -> Grafo a
lineal = foldr (\n rec ->  if null (nodos rec)
						   then agNodo n rec
						   else agEje (n,head (nodos rec)) (agNodo n rec)
			   )
			   vacio

-- Ejercicio 8
union :: Grafo a -> Grafo a -> Grafo a
union = undefined

-- Ejercicio 9
clausura :: Grafo a -> Grafo a
clausura = undefined





