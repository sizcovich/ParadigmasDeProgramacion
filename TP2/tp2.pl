%%%%%%%%%%%%%%%%%%%%%%%%
%% Tablero
%%%%%%%%%%%%%%%%%%%%%%%%
%% Ejercicio 1
%% tablero(+Filas,+Columnas,-Tablero) instancia una estructura de tablero en blanco
%% de Filas x Columnas, con todas las celdas libres.
tablero(0,_,[]).
tablero(F,C,[X|Xs]) :- F > 0, length(X, C), Y is F-1, tablero(Y, C, Xs).
%% Genera una lista de F listas (de longitud C). Verificamos que F>0 para que no se itere sobre valores negativos y
%% asegurarnos de que termine pasando por la primer línea cuando F=0.

%% Ejercicio 2
%% ocupar(+Pos,?Tablero) será verdadero cuando la posición indicada esté ocupada.
ocupar(Pos,T) :- posicion(Pos, T, ocupada).
%% Instancia la posición Pos como ocupada.

%% posicion(pos(+X,+Y),?Tablero,?Elem) es verdadero si Elem es el elemento (X,Y) del tablero T.
posicion(pos(X, Y), T, Elem) :- var(Elem), nth0(X, T, Fila), nth0(Y, Fila, ElemT), var(ElemT).
posicion(pos(X, Y), T, Elem) :- nonvar(Elem), nth0(X, T, Fila), nth0(Y, Fila, ocupada).
%% Si pos(X,Y) no está instanciada (se verifica con var), el elemento de dicha posición debe ser una variable.
%% Si la posición (X,Y) está instanciada (se verifica con nonvar) significa que está ocupada, entonces simplemente se 
%% pide que el valor (X,Y) sea igual a ocupada.


%% Ejercicio 3
%% vecino(+Pos, +Tablero, ?PosVecino) será verdadero cuando PosVecino sea
%% un átomo de la forma pos(F', C') y pos(F',C') sea una celda contigua a
%% pos(F,C), donde Pos=pos(F,C). Las celdas contiguas puede ser a lo sumo cuatro
%% dado que el robot se moverá en forma ortogonal.
vecino(pos(X1,Y1),[T|Ts],pos(V1,V2)) :- length([T|Ts],Alto),length(T,Ancho),
							between(-1,1,Iterador1),
							V1 is X1+Iterador1, V1 < Alto, V1 >= 0,
							between(-1,1,Iterador2),
							V2 is Y1+Iterador2, V2 < Ancho, V2 >= 0,
							AbsIt1 is abs(Iterador1), AbsIt2 is abs(Iterador2), SumAbs is AbsIt1 + AbsIt2, SumAbs = 1.
%% En este ejercicio usamos Generate&Test creando, en primer lugar, todos los posibles valores de Iterador1 para evaluarlos 
%% más tarde y, creando luego, todos los valores de Iterador2 para evaluarlos también.
%% Para este ejercicio, tuvimos en cuenta que se considera vecino al elemento movido una coordenada para
%% alguna de las siguientes direcciones: derecha, izquierda, arriba o abajo. Para eso, sumamos o restamos 0 o 1 a ambas 
%% coordenadas pidiendo que el valor absoluto del total de lo que se suma o resta sea igual a 1. De este modo nos aseguramos 
%% que sea sólo una coordenada la afectada.

%% Ejercicio 4
%% vecinoLibre(+Pos, +Tablero, ?PosVecino) idem vecino/3 pero además PosVecino
%% debe ser una celda transitable (no ocupada) en el Tablero
vecinoLibre(Pos,T,PosVecino) :- vecino(Pos,T,PosVecino), posicion(PosVecino,T,_).
%% En este caso se toman los vecinos de Pos y se pide que sus respectivas posiciones no estén instanciadas. De este 
%% modo nos aseguramos de que están libres.

%%%%%%%%%%%%%%%%%%%%%%%%
%% Definicion de caminos
%%%%%%%%%%%%%%%%%%%%%%%%

%% Ejercicio 5
%% camino(+Inicio, +Fin, +Tablero, -Camino) será verdadero cuando Camino sea una lista
%% [pos(F1,C1), pos(F2,C2),..., pos(Fn,Cn)] que denoten un camino desde Inicio
%% hasta Fin pasando solo por celdas transitables.
%% Además se espera que Camino no contenga ciclos.
%% Notar que la cantidad de caminos es finita y por ende se tiene que poder recorrer
%% todas las alternativas eventualmente.
%% Consejo: Utilizar una lista auxiliar con las posiciones visitadas
camino(I,F,T,C) :- caminoConPosicionesVisitadas(I,F,T,[],C).

%% caminoConPosicionesVisitadas(+Inicio, +Fin, +Tablero, +Lista, -Camino)
caminoConPosicionesVisitadas(F,F,_,_,[F]). %% Llegamos a Inicial = Final.
caminoConPosicionesVisitadas(I,F,T,H,[I|Cs]) :- vecinoLibre(I,T,V), not(member(V,H)), caminoConPosicionesVisitadas(V,F,T,[I|H],Cs).
%% Se almacenan las posiciones visitadas en un arreglo de forma tal a poder evaluar si ya fue recorrida o no al momento de llegar
%% a ésta. Para eso, se pide que no pertenezca al arreglo formado (utilizando not(member)).

%% Ejercicio 6
%% cantidadDeCaminos(+Inicio, +Fin, +Tablero, ?N) que indique la cantidad de caminos
%% posibles sin ciclos entre Inicio y Fin. 

cantidadDeCaminos(I,I,_,1).
cantidadDeCaminos(I, F, T, N) :- aggregate_all(count, camino(I,F,T,_), N).

%% Ejercicio 7
%% camino2(+Inicio, +Fin, +Tablero, -Camino) ídem camino/4 pero se espera una heurística
%% que mejore las soluciones iniciales.
%% No se espera que la primera solución sea necesariamente la mejor.
%% Una solución es mejor mientras menos pasos se deba dar para llegar a
%% destino (distancia Manhattan). Por lo tanto, el predicado deberá devolver de a uno,
%% todos los caminos pero en orden creciente de longitud.
camino2(I,F,T,C) :- caminoConPosicionesVisitadas2(I,F,T,[],C).

%% caminoConPosicionesVisitadas2(+Inicio, +Fin, +Tablero, +Lista, -Camino)
%% Observar que la definición es idéntica a la de camino, pero los vecinos libres
%% se recorren en orden desde el más cercano al destino hasta el más lejano.
caminoConPosicionesVisitadas2(F,F,_,_,[F]). %% Llegamos a Inicial = Final.
caminoConPosicionesVisitadas2(I,F,T,H,[I|Cs]) :-
				vecinoLibreOrdenDistManhattan(I,F,T,V),
				not(member(V,H)),
				caminoConPosicionesVisitadas2(V,F,T,[I|H],Cs).
				
%% vecinoLibreOrdenDistManhattan(+Inicio,+Fin,+Tablero,-Vecino)
%% La idea de este predicado es que el último parámetro vaya instanciado vecinos
%% de "Inicio" en el orden desde el que está mas cerca hasta el que está mas lejos
%% de Fin en distancia Manhattan.
vecinoLibreOrdenDistManhattan(I,F,T,V):-
				dimensiones(T,CantFilas,CantCols),
				DistMax is CantFilas * CantCols,       %% Como máximo la distancia desde un vecino hasta el Fin es recorrer todo el tablero.
				                                       %% (podría haber varias idas y vueltas según qué posiciones estén ocupadas)
				between(0,DistMax,Dist),               %% Probamos todas las distancias posibles (desde cero porque V podría ser F)
				vecinoLibre(I,T,V),                    %% Por cada vecino libre de I
				distManhattan(V,F,Dist).               %% que esté a distancia Dist
%% Los vecinos se recorren en orden desde menor distancia a F.
%% No genera repetidos porque cada vecino está a una única distancia de F
%% y las distancias Dist no se repiten.


%% distManhattan(+Inicio,+Fin,?Distancia)
%% Observar que como las posiciones del tablero son enteros, entonces la distancia siempre es entera.
distManhattan(pos(X1,Y1),pos(X2,Y2),D):- D is abs(X1-X2)+abs(Y1-Y2).

%% dimensiones(+Tablero, -CantidadFilas, -CantidadColumnas)
%% Tiene como precondición que el tablero esté bien formado (al menos 1 fila y 1 columna)
dimensiones([Fila1|T],F,C):- length([Fila1|T],F), length(Fila1,C).
		
% cantidadDeCaminos2(+Inicio, +Fin, +Tablero, -N)  (solo para corroborar el ejemplo del enunciado)
cantidadDeCaminos2(Inicio,Fin,T,N):- aggregate_all(count, camino2(Inicio,Fin,T,_), N).
		
%% Ejercicio 8
%% camino3(+Inicio, +Fin, +Tablero, -Camino) ídem camino2/4 pero se espera que
%% se reduzca drásticamente el espacio de búsqueda.
%% En el proceso de generar los potenciales caminos, se pueden ir sacando algunas conclusiones.
%% Por ejemplo, si se está en la celda (3,4) y se dieron ya 6 pasos desde el Inicio,
%% entonces no tiene sentido seguir evaluando cualquier camino que implique llegar a la celda (3,4)
%% desde Inicio en más de 6 pasos.
%% Notar que dos ejecuciones de camino3/4 con los mismos argumentos deben dar los mismos resultados.
%% En este ejercicio se permiten el uso de predicados: dynamic/1, asserta/1, assertz/1 y retractall/1.

%% Declaración del predicado dinámico (para poder agregar y sacar reglas)
:- dynamic caminoLongitud/1.

%% caminoDeLongitudMenorA(+Longitud)
%% Devuelve true cuando existe algún camino de longitud menor a Longitud entre los
%% encontrados hasta el momento. Dado que los caminos encontrados
%% se van poniendo arriba en la lista de reglas del predicado caminoLongitud/1,
%% y además siempre se encuentran caminos de longitud igual o más chica, entonces
%% la primer instanciación de caminoLongitud(L) es tal que L es la longitud mínima.
caminoDeLongitudMenorA(X):- caminoLongitud(L), L < X, !.  % el cut obliga a que Prolog mire sólo el primer camino (el más corto).

%% noHayCaminoDeLongitudMenorA(+Longitud)
%% Da true si no existe ningún camino de longitud menor a Longitud encontrado
%% hasta el momento
noHayCaminoDeLongitudMenorA(X):- not(caminoDeLongitudMenorA(X)).

%% retractall borra todas las reglas del predicado pasado por parámetro y lo inicializa.
camino3(I,F,T,C) :- retractall(caminoLongitud(_)), caminoConPosicionesVisitadas3(I,F,T,[],C).

%% caminoConPosicionesVisitadas3(+Inicio, +Fin, +Tablero, +Lista, -Camino)
%% Observar que la definición es idéntica a la de camino2
caminoConPosicionesVisitadas3(F,F,_,Recorridos,[F]):-
			length(Recorridos, Longit),
			L is Longit+1,                %% (Los recorridos hasta el predicado que me llama a mí) + F (la última celda).
			asserta(caminoLongitud(L)).   %% Se agrega esta como LA PRIMER REGLA. Entonces caminoLongitud(X) siempre devuelve el camino
			                              %% de longitud menor hasta ahora encontrado (si se llega al caso base siempre es por un camino
										  %% de longitud igual o menor al encontrado hasta el momento)

caminoConPosicionesVisitadas3(I,F,T,Recorridos,[I|Cs]) :-
			length(Recorridos, Longit),             % Los recorridos hasta ahora
			L is Longit + 2,                        % Inicio y V seguro van a formar parte del camino resultante, ya que Inicio!=Fin
			noHayCaminoDeLongitudMenorA(L),
			vecinoLibreOrdenDistManhattan(I,F,T,V),
			not(member(V,Recorridos)),
			caminoConPosicionesVisitadas3(V,F,T,[I|Recorridos],Cs).


% cantidadDeCaminos3(+Inicio, +Fin, +Tablero, -N)  (solo para corroborar el ejemplo del enunciado)
cantidadDeCaminos3(Inicio,Fin,T,N):- aggregate_all(count, camino3(Inicio,Fin,T,_), N).


%%%%%%%%%%%%%%%%%%%%%%%%
%% Tableros simultáneos
%%%%%%%%%%%%%%%%%%%%%%%%

%% Ejercicio 9
%% caminoDual(+Inicio, +Fin, +Tablero1, +Tablero2, -Camino) será verdadero
%% cuando Camino sea un camino desde Inicio hasta Fin pasando al mismo tiempo
%% sólo por celdas transitables de ambos tableros.
%% Nota: Es posible una implementación que resuelva en forma inmediata casos en los que trivialmente no existe camino dual posible.
caminoDual(I,F,T1,T2,C) :- camino(I,F,T1,C), camino(I,F,T2,C).
%% La función evalúa que el camino C del tablero T1 coincida con las soluciones del tablero T2.

%%%%%%%%%%%%%%%%%%%%%%%%
%% Tableros ejemplo
%%%%%%%%%%%%%%%%%%%%%%%%

%aca se testea tablero y ocupar.
tablero(ej5x5, T) :- tablero(5, 5, T), ocupar(pos(1, 1), T), ocupar(pos(1, 2), T).
%%| | | | | |
%%| |O|O| | |
%%| | | | | |
%%| | | | | |
tablero(ocupadasDelMedio, T) :- tablero(4, 4, T), ocupar(pos(1, 1), T), ocupar(pos(1, 2), T), ocupar(pos(2, 1), T), ocupar(pos(2, 2), T).
%%| | | | |
%%| |O|O| |
%%| |O|O| |
%%| | | | |
tablero(unaOcupada, T):- tablero(3, 2, T), ocupar(pos(1, 0), T).
%%| | |
%%|O| |
%%| | |
tablero(libre20, T) :- tablero(20, 20, T).
tablero(libre3x2, T) :- tablero(3, 2, T).

%-----------------
%----- Tests -----
%-----------------
%%%% Para correrlos: tests.

%vecino
test(1) :- tablero(unaOcupada, T), vecino(pos(0,0),T,pos(1,0)).
test(2) :- tablero(unaOcupada, T), not(vecino(pos(0,0),T,pos(1,1))).
test(3) :- tablero(unaOcupada, T), not(posicion(vecino(pos(1,0),T,pos(0,0)),T,ocupada)).
%vecinoLibre
test(4) :- tablero(ej5x5, T), vecinoLibre(pos(0,0), T, pos(0,1)).
test(5) :- tablero(unaOcupada, T), not(vecinoLibre(pos(0,0), T, pos(1,0))).
test(6) :- tablero(libre20, T), vecinoLibre(pos(10,10),T,X), X = pos(9, 10).
test(7) :- tablero(libre20, T), vecinoLibre(pos(10,10),T,X), X = pos(10, 9).
test(8) :- tablero(libre20, T), vecinoLibre(pos(10,10),T,X), X = pos(10, 11).
test(9) :- tablero(libre20, T), vecinoLibre(pos(10,10),T,X), X = pos(11, 10).
%camino
test(10) :- tablero(libre3x2, T), camino(pos(0,0), pos(1,1),T,C), C = [pos(0, 0), pos(0, 1), pos(1, 1)].
test(11) :- tablero(libre3x2, T), camino(pos(0,0), pos(1,1),T,C), C = [pos(0, 0), pos(1, 0), pos(1, 1)].
test(12) :- tablero(libre3x2, T), camino(pos(0,0), pos(1,1),T,C), C = [pos(0, 0), pos(1, 0), pos(2, 0), pos(2, 1), pos(1, 1)].
test(13) :- tablero(ej5x5, T), camino(pos(0,0), pos(2,3), T, C), C =[pos(0, 0), pos(0, 1), pos(0, 2), pos(0, 3), pos(0, 4), pos(1, 4), pos(1, 3), pos(2, 3)].
%cantidadDeCaminos
test(14) :- tablero(ej5x5, T), cantidadDeCaminos(pos(0,0), pos(2,3), T, N), N = 287.
test(15) :- tablero(ocupadasDelMedio, T), cantidadDeCaminos(pos(0,0),pos(1,0),T,N), N = 2.
%camino2
test(16) :- tablero(ej5x5, T), camino2(pos(0,0), pos(2,3), T, C), C = [pos(0, 0), pos(1, 0), pos(2, 0), pos(2, 1), pos(2, 2), pos(2, 3)].
test(17) :- tablero(libre3x2, T), camino2(pos(0,0), pos(1,1),T,C), C = [pos(0, 0), pos(0, 1), pos(1, 1)].
test(18) :- tablero(libre3x2, T), camino2(pos(0,0), pos(1,1),T,C), C = [pos(0, 0), pos(1, 0), pos(1, 1)].
test(19) :- tablero(libre3x2, T), camino2(pos(0,0), pos(1,1),T,C), C = [pos(0, 0), pos(1, 0), pos(2, 0), pos(2, 1), pos(1, 1)].
test(20) :- tablero(ej5x5, T), cantidadDeCaminos2(pos(0,0), pos(2,3), T, N), N = 287.
%camino3
test(21) :- tablero(ej5x5, T), camino3(pos(0,0), pos(2,3), T, C), C =[pos(0, 0), pos(1, 0), pos(2, 0), pos(2, 1), pos(2, 2), pos(2, 3)].
test(22) :- tablero(ej5x5, T), cantidadDeCaminos3(pos(0,0), pos(2,3), T, N), N=2.
%caminoDual
test(23) :- tablero(ej5x5, T), caminoDual(pos(0,0),pos(2,3),T,T,C), C = [pos(0, 0), pos(0, 1), pos(0, 2), pos(0, 3), pos(0, 4), pos(1, 4), pos(1, 3), pos(2, 3)].
test(24) :- tablero(ej5x5, T1), tablero(ocupadasDelMedio, T2), caminoDual(pos(0,0),pos(3,3),T1,T2,C), C = [pos(0, 0), pos(0, 1), pos(0, 2), pos(0, 3), pos(1, 3), pos(2, 3), pos(3, 3)].
test(25) :- tablero(ej5x5, T1), tablero(ocupadasDelMedio, T2), caminoDual(pos(0,0),pos(3,3),T1,T2,C), C = [pos(0, 0), pos(1, 0), pos(2, 0), pos(3, 0), pos(3, 1), pos(3, 2), pos(3, 3)].

tests :- forall(between(1, 25, N), test(N)).
