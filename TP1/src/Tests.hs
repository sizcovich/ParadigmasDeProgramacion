import Grafo
import Tipos
import Lomoba
import Parser
import Test.HUnit

-- evaluar t para correr todos los tests
t = runTestTT allTests

allTests = test [
	"parser" ~: testsParser,
	"grafo" ~: testsGrafo,
	"lomoba" ~: testsLomoba
	]

testsParser = test [
	(Var "p") 						~=? (parse "p"),
	(And (Var "p") (Var "q")) 		~=? (parse "p && q"),
	(Or (Var "p") (Var "q")) 		~=? (parse "p || q"),
	(Or (Not (Var "p")) (Var "q"))	~=? (parse "!p || q"),
	(And (D (Var "p")) (Var "q")) 	~=? (parse "<>p && q"),
	(And (B (Var "p")) (Var "q")) 	~=? (parse "[]p && q"),
	(D (And (Var "p") (Var "q"))) 	~=? (parse "<>(p && q)"),
	(B (And (Var "p") (Var "q"))) 	~=? (parse "[](p && q)")]

testsGrafo = test [
	-- Ej 1,2,4 (agregar nodos, ver nodos, grafo vacío)
	[1] ~~? (nodos (agNodo 1 vacio)),
	[1,2] ~~? (nodos (agNodo 2 (agNodo 1 vacio))),
	[1,2] ~~? (nodos (agNodo 2 (agNodo 2 (agNodo 1 vacio)))),
	-- Ej 3,6 (agrega ejes, ver vecinos)
	[] ~~? (vecinos (agNodo 3 (agNodo 2 (agNodo 1 vacio))) 1),
	[] ~~? (vecinos (agNodo 3 (agNodo 2 (agNodo 1 vacio))) 5), -- es total
	[2] ~~? (vecinos (agEje (3,2) (agNodo 3 (agNodo 2 (agNodo 1 vacio)))) 3),
	[2,3] ~~? (vecinos (agEje (3,3) (agEje (3,2) (agNodo 3 (agNodo 2 (agNodo 1 vacio))))) 3),
	[2] ~~? (vecinos (agEje (3,2) (agEje (3,2) (agNodo 3 (agNodo 2 (agNodo 1 vacio))))) 3),
	-- Ej 5 (sacar nodo)
	[1,3] ~~? nodos (sacarNodo 2 (agNodo 3 (agNodo 2 (agNodo 1 vacio)))),
	[1] ~~? vecinos (sacarNodo 2 (agEje (3,2) (agEje (3,1) (agNodo 3 (agNodo 2 (agNodo 1 vacio)))))) 3,
	[] ~~? vecinos (sacarNodo 2 (agNodo 3 (agNodo 2 (agNodo 1 vacio)))) 2, -- sacar nodo mantiene que sea funcion total
	-- Ej 7 (lineal)
	(agEje (2,3) (agEje (1,2) (agNodo 3 (agNodo 2 (agNodo 1 vacio))))) ~=? lineal [1,2,3],
	-- Ej 8 (union de grafos)
	(agEje (3,4) (agEje (1,2) (agNodo 4 (agNodo 3 (agNodo 2 (agNodo 1 vacio))))))
						~=? union (agEje (3,4) (agNodo 4 (agNodo 3 vacio)))
						          (agEje (1,2)(agNodo 2 (agNodo 1 vacio))), -- Grafos disjuntos
	(agEje (1,2) (agEje (1,3) (agEje (2,3) (agNodo 3 (agNodo 2 (agNodo 1 vacio))))))
						~=? union (agEje (1,2) (agNodo 1 (agNodo 2 vacio)))
								  (agEje (1,3) (agEje (2,3) (agNodo 3 (agNodo 2 (agNodo 1 vacio))))), -- Algunos nodos en común
	(agEje (1,3) (agEje (2,3) (agNodo 3 (agNodo 2 (agNodo 1 vacio)))))
				        ~=? union (agEje (1,3) (agEje (2,3) (agNodo 3 (agNodo 2 (agNodo 1 vacio)))))
								  (agEje (1,3) (agEje (2,3) (agNodo 3 (agNodo 2 (agNodo 1 vacio))))), -- Grafos idénticos
	(agEje (1,3) (agEje (2,3) (agNodo 3 (agNodo 2 (agNodo 1 vacio)))))
				        ~=? union (agEje (1,3) (agNodo 1 (agEje (2,3) (agNodo 3 (agNodo 2 vacio)))))
								  (agEje (1,3) (agEje (2,3) (agNodo 3 (agNodo 2 (agNodo 1 vacio))))), -- Grafos idénticos en distinto orden
	-- Ej 9 (clausura transitiva)
	-- hago un grafo que es un ciclo y deberia obtener un completo con la clausura
	[1,2,3,4] ~~? vecinos (clausura (agEje (4,1) (lineal [1,2,3,4]))) 1,
	[1,2,3,4] ~~? vecinos (clausura (agEje (4,1) (lineal [1,2,3,4]))) 2,
	[1,2,3,4] ~~? vecinos (clausura (agEje (4,1) (lineal [1,2,3,4]))) 3,
	[1,2,3,4] ~~? vecinos (clausura (agEje (4,1) (lineal [1,2,3,4]))) 4
	]
	
testsLomoba = test [
	-- Ej 13
	True ~=? eval modeloKrEnunciado 1 (parse "p&&[]q"),
	True ~=? eval modeloKrEnunciado 1 (parse "p&&<>r"),
	False ~=? eval modeloKrEnunciado 1 (parse "[]r"),
	True ~=? eval modeloKrEnunciado 1 (parse "<>(q&&r)")
	]
	

-- El grafo del enunciado
modeloKrEnunciado = K (agEje (1,2) (agEje (1,3) (agNodo 3 (agNodo 2 (agNodo 1 vacio)))))
						 (\p -> case () of
									_ | p=="p" -> [1]
									  | p=="q" -> [2,3]
									  | p=="r" -> [3])

---------------
--  helpers  --
---------------

-- idem ~=? pero sin importar el orden
(~~?) :: (Ord a, Eq a, Show a) => [a] -> [a] -> Test
expected ~~? actual = (sort expected) ~=? (sort actual)
	where
		sort = foldl (\r e -> push r e) []
		push r e = (filter (e<=) r) ++ [e] ++ (filter (e>) r)
