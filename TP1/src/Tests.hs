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
	-- Ej 1,2
	[1] ~~? (nodos (agNodo 1 vacio)),
	[1,2] ~~? (nodos (agNodo 2 (agNodo 1 vacio))),
	-- Ej 5
	[1,3] ~~? nodos (sacarNodo 2 (agNodo 3 (agNodo 2 (agNodo 1 vacio)))),
	[1] ~~? vecinos (sacarNodo 2 (agEje (3,2) (agEje (3,1) (agNodo 3 (agNodo 2 (agNodo 1 vacio)))))) 3,
	-- Ej 7
	(agEje (2,3) (agEje (1,2) (agNodo 3 (agNodo 2 (agNodo 1 vacio))))) ~=? lineal [1,2,3]
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
