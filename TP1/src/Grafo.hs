module Grafo (Grafo, vacio, nodos, vecinos, agNodo, sacarNodo, agEje, lineal, union, clausura) where

data Grafo a = G [a] (a -> [a])

instance (Show a) => Show (Grafo a) where
	show (G n e) = "[\n" ++ concat (map (\x -> " " ++ show x ++ " -> " ++ show (e x) ++ "\n") n) ++ "]"

instance (Eq a) => Eq (Grafo a) where
	(G n1 e1) == (G n2 e2) = (listasIguales n1 n2) && (all (\n -> (listasIguales (e1 n) (e2 n))) n1)

-- Igualdad de listas sin importar el orden
listasIguales :: (Eq a) => [a] -> [a] -> Bool	
listasIguales l1 l2 = (all (\x -> x `elem` l1) l2) && (all (\x -> x `elem` l2) l1)

-- ---------------------------------Sección 3--------- Grafos ---------------------------

-- Ejercicio 1
-- Crea un nuevo grafo con una lista vacía de nodos y una función que
-- devuelve siempre [] (es decir que si luego pedimos los vecinos de
-- cualquier nodo que no esté en el grafo, da una lista vacía. Esto es
-- para que la función sea total.
vacio :: Grafo a
vacio = G [] (const [])

-- Ejercicio 2
nodos :: Grafo a -> [a]
nodos (G ns ejes) = ns

-- Ejercicio 3
vecinos :: Grafo a -> a -> [a]
vecinos (G ns ejes) = \y -> ejes y

-- Ejercicio 4
agNodo :: Eq a => a -> Grafo a -> Grafo a
agNodo x (G ns t) = if x `elem` ns then (G ns t) else (G (x:ns) t)

-- Ejercicio 5
-- Construye un nuevo grafo:
--    * Filtra la lista de nodos para sacar el nodo.
--    * Crea una nueva función que se indefine para el nodo que acabamos
--      de sacar y para los demás nodos devuelve los mismos vecinos de
--      antes salvo el nodo que se sacó.
sacarNodo :: Eq a => a -> Grafo a -> Grafo a
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
lineal :: Eq a => [a] -> Grafo a
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





