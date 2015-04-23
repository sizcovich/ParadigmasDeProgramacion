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
	[2,3] ~~? (vecinos (agEje (3,3) (agEje (3,2) 
                (agNodo 3 (agNodo 2 (agNodo 1 vacio))))) 3),
	[2] ~~? (vecinos (agEje (3,2) (agEje (3,2) (agNodo 3 (agNodo 2 (agNodo 1 vacio))))) 3),
	[1,2,3,4] ~~? (vecinos (agEje (5,1) (agEje (5,2) (agEje (5,3) (agEje (5,4) 
                    (agNodo 5 (agNodo 4 (agNodo 3 (agNodo 2 (agNodo 1 vacio))))))))) 5),
        
	-- Ej 5 (sacar nodo)
	[1,3] ~~? nodos (sacarNodo 2 (agNodo 3 (agNodo 2 (agNodo 1 vacio)))),
	[1,2,3] ~~? nodos (agNodo 2 (sacarNodo 2 
                        (agNodo 3 (agNodo 2 (agNodo 1 vacio))))),
	[1] ~~? vecinos (sacarNodo 2 (agEje (3,2) (agEje (3,1) (agNodo 3 (agNodo 2 (agNodo 1 vacio)))))) 3,
	[] ~~? vecinos (sacarNodo 2 (agNodo 3 (agNodo 2 (agNodo 1 vacio)))) 2,
	
	-- Ej 7 (lineal)
	(agEje (2,3) (agEje (1,2) (agNodo 3 (agNodo 2 (agNodo 1 vacio))))) ~=? lineal [1,2,3],

	-- Ej 8 (union de grafos)
	-- Grafos disjuntos
	(agEje (3,4) (agEje (1,2) (agNodo 4 (agNodo 3 (agNodo 2 (agNodo 1 vacio))))))
						~=? union (agEje (3,4) (agNodo 4 (agNodo 3 vacio)))
						          (agEje (1,2)(agNodo 2 (agNodo 1 vacio))),
    -- Grafo vacio
    (agEje (1,2) (agNodo 2 (agNodo 1 vacio)))
						~=? union vacio
						          (agEje (1,2)(agNodo 2 (agNodo 1 vacio))),
    -- Grafos lineales
	(lineal [1,2,3,4,5,6]) ~=? union (lineal [1,2,3]) (lineal [3,4,5,6]),
    -- Algunos nodos en común
	(agEje (1,2) (agEje (1,3) (agEje (2,3) (agNodo 3 (agNodo 2 (agNodo 1 vacio))))))
						~=? union (agEje (1,2) (agNodo 1 (agNodo 2 vacio)))
								  (agEje (1,3) (agEje (2,3) 
                                        (agNodo 3 (agNodo 2 (agNodo 1 vacio))))),
    
    -- Grafos idénticos
	(agEje (1,3) (agEje (2,3) (agNodo 3 (agNodo 2 (agNodo 1 vacio)))))
				        ~=? union (agEje (1,3) (agEje (2,3) 
                                        (agNodo 3 (agNodo 2 (agNodo 1 vacio)))))
								  (agEje (1,3) (agEje (2,3) 
                                        (agNodo 3 (agNodo 2 (agNodo 1 vacio))))),
    -- Grafos idénticos en distinto orden
	(agEje (1,3) (agEje (2,3) (agNodo 3 (agNodo 2 (agNodo 1 vacio)))))
				        ~=? union (agEje (1,3) (agNodo 1 (agEje (2,3) 
                                        (agNodo 3 (agNodo 2 vacio)))))
								  (agEje (1,3) (agEje (2,3) 
                                        (agNodo 3 (agNodo 2 (agNodo 1 vacio))))),
	
	-- Ej 9 (clausura transitiva)
	-- hago un grafo que es un ciclo y deberia obtener un completo con la clausura
	[1,2,3,4] ~~? vecinos (clausura (agEje (4,1) (lineal [1,2,3,4]))) 1,
	[1,2,3,4] ~~? vecinos (clausura (agEje (4,1) (lineal [1,2,3,4]))) 2,
	[1,2,3,4] ~~? vecinos (clausura (agEje (4,1) (lineal [1,2,3,4]))) 3,
	[1,2,3,4] ~~? vecinos (clausura (agEje (4,1) (lineal [1,2,3,4]))) 4
	]
	
testsLomoba = test [
	-- Ej 11
	0 ~=? visibilidad (parse "p"),
	1 ~=? visibilidad (parse "<>p"),
	2 ~=? visibilidad (parse "<>!(<>p)"),
	2 ~=? visibilidad (parse "<><>p||<><>q"),
	3 ~=? visibilidad (parse "<>(<>p||<><>q)"),
	3 ~=? visibilidad (parse "[](<>p&&<>[]q)"),
	2 ~=? visibilidad (parse "<><>p||<><>q||<><>r"),
	0 ~=? visibilidad (parse "p||q||r||s||t"),
	10 ~=? visibilidad (parse "[]<>[]<>[]<>[]<>[]<>p"),
	4 ~=? visibilidad (parse "[][][](p||[]q||r||<>s||t)"),

	-- Ej 12
	["p"] ~=? extraer (parse "p"),
	["p"] ~=? extraer (parse "<>p"),
	["p"] ~=? extraer (parse "<>!<>p"),
	["p","q"] ~=? extraer (parse "<><>p||<><>q"),
	["p","q"] ~=? extraer (parse "<>(<>p||<><>q)"),
	["p","q"] ~=? extraer (parse "[](<>p&&<>[]q)"),
	["p","q","r"] ~=? extraer (parse "<><>p||<><>q||<><>r"),
	["p","q","r","s","t"] ~=? extraer (parse "p||q||r||s||t"),
	["p"] ~=? extraer (parse "<>[]<>[]<>[]<>[]<>p"),
	["p","q","r","s","t"] ~=? extraer (parse "[][][](p||[]q||r||<>s||t)"),

	-- Ej 13
	True ~=? eval modeloKrEnunciado 1 (parse "p&&[]q"),
	True ~=? eval modeloKrEnunciado 1 (parse "p&&<>r"),
	False ~=? eval modeloKrEnunciado 1 (parse "[]r"),
	True ~=? eval modeloKrEnunciado 1 (parse "<>(q&&r)"),
	False ~=? eval modeloKr1 1 (parse "<>(q&&r)"),
	True ~=? eval modeloKr1 1 (parse "<>(<>r)"),
	True ~=? eval ciclo 2 (parse "<>q"),

	-- Ej 14
	[1] ~=? valeEn (parse "p") modeloKrEnunciado, 
	[5,4,3,2,1] ~=? valeEn (parse "<>p") ciclo,
	[] ~=? valeEn (parse "<>!<>p") ciclo,
	[3,2,1] ~=? valeEn (parse "<>r||<>q") modeloKr1,
	[5,4,3,2,1] ~=? valeEn (parse "<>(<>p||<><>q)") ciclo,
	[5,4,3,2] ~=? valeEn (parse "!p||q||r") modeloKr1,
	
    -- Ej 15
	[1] ~~? (\(K g f) -> nodos g)(quitar (parse "p&&[]q") modeloKrEnunciado),
	[] ~~? ((\(K g f) -> f)(quitar (parse "p&&[]q") modeloKrEnunciado)) "q",
	[1] ~~? ((\(K g f) -> f)(quitar (parse "p&&[]q") modeloKrEnunciado)) "p",
	
	
	
	[2,3] ~~? (\(K g f) -> nodos g)(quitar (parse "q") modeloKrEnunciado),
	[3] ~~? ((\(K g f) -> f)(quitar (parse "q") modeloKrEnunciado)) "r",
	[2,3] ~~? ((\(K g f) -> f)(quitar (parse "q") modeloKrEnunciado)) "q",
	
	(\(K g f) -> g)modeloKrEnunciado ~=? 
                (\(K g f) -> g)(quitar (parse "q||<>r") modeloKrEnunciado),

	-- Ej 16
	True ~=? cierto ciclo (parse "p"),
	False ~=? cierto ciclo (parse "q"),
	False ~=? cierto modeloKrEnunciado (parse "q"),
	False ~=? cierto modeloKrEnunciado (parse "p&&r || q"),
	True ~=? cierto modeloKrEnunciado (parse "p || q || r"),
	True ~=? cierto ciclo (parse "p || q || r"),
	True ~=? cierto modeloKr1 (parse "p || q || r"),
	False ~=? cierto modeloKr1 (parse "[]p"),
	False ~=? cierto ciclo (parse "<>(q&&r)")
	]
	

-- El grafo del enunciado
modeloKrEnunciado = K (agEje (1,2) (agEje (1,3) (agNodo 3 (agNodo 2 (agNodo 1 vacio)))))
						 (\p -> case () of
									_ | p=="p" -> [1]
									  | p=="q" -> [2,3]
									  | p=="r" -> [3]
									  | otherwise -> []) -- debe ser total

modeloKr1 = K (agEje (1,2) (agEje (1,3) (agEje (3,4) (agEje (2,5) 
                (agNodo 5 (agNodo 4 (agNodo 3 (agNodo 2 (agNodo 1 vacio)))))))))
						 (\p -> case () of
									_ | p=="p" -> [1]
									  | p=="q" -> [2,3]
									  | p=="r" -> [4,5]
									  | otherwise -> []) -- debe ser total

ciclo = K (agEje (1,2) (agEje (2,3) (agEje (3,4) (agEje (4,5) (agEje (5,1) 
                (agNodo 5 (agNodo 4 (agNodo 3 (agNodo 2 (agNodo 1 vacio))))))))))
						 (\p -> case () of
									_ | p=="p" -> [1, 2, 3, 4, 5]
									  | p=="q" -> [3]
									  | p=="r" -> [4]
									  | otherwise -> []) -- debe ser total

---------------
--  helpers  --
---------------

-- idem ~=? pero sin importar el orden
(~~?) :: (Ord a, Eq a, Show a) => [a] -> [a] -> Test
expected ~~? actual = (sort expected) ~=? (sort actual)
	where
		sort = foldl (\r e -> push r e) []
		push r e = (filter (e<=) r) ++ [e] ++ (filter (e>) r)
