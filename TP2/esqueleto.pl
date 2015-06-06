%%%%%%%%%%%%%%%%%%%%%%%%
%% Tableros ejemplo
%%%%%%%%%%%%%%%%%%%%%%%%
tablero(ej5x5, T) :- tablero(5, 5, T), ocupar(pos(1, 1), T), ocupar(pos(1, 2), T).

tablero(unaOcupada, T):- tablero(3, 2, T), ocupar(pos(1, 0), T).
tablero(libre20, T) :- tablero(20, 20, T).
tablero(libre3x2, T) :- tablero(3, 2, T).

%%%%%%%%%%%%%%%%%%%%%%%%
%% Tablero
%%%%%%%%%%%%%%%%%%%%%%%%
%% Ejercicio 1
%% tablero(+Filas,+Columnas,-Tablero) instancia una estructura de tablero en blanco
%% de Filas x Columnas, con todas las celdas libres.
tablero(0,_,[]).
tablero(F,C,[X|Xs]) :- length(X, C), Y is F-1, tablero(Y, C, Xs), !.


%% Ejercicio 2
%% ocupar(+Pos,?Tablero) será verdadero cuando la posición indicada esté ocupada.
ocupar(Pos,T) :- posicion(Pos, T, ocupada).

%% posicion(pos(+X,+Y),?Tablero,?Elem) es verdadero si Elem es el elemento (X,Y) del tablero T.
posicion(pos(X, Y), T, Elem) :- var(Elem), nth0(X, T, Fila), nth0(Y, Fila, ElemT), var(ElemT).
posicion(pos(X, Y), T, Elem) :- nonvar(Elem), nth0(X, T, Fila), nth0(Y, Fila, ocupada).

%% Ejercicio 3
%% vecino(+Pos, +Tablero, -PosVecino) será verdadero cuando PosVecino sea
%% un átomo de la forma pos(F', C') y pos(F',C') sea una celda contigua a
%% pos(F,C), donde Pos=pos(F,C). Las celdas contiguas puede ser a lo sumo cuatro
%% dado que el robot se moverá en forma ortogonal.
vecino(pos(X1,Y1),[T|Ts],pos(V1,V2)) :- length([T|Ts],Alto),length(T,Ancho),
							between(-1,1,Iterador1), between(-1,1,Iterador2),
							V1 is X1+Iterador1, V1 < Alto, V1 >= 0, 
							V2 is Y1+Iterador2, V2 < Ancho, V2 >= 0,
							AbsIt1 is abs(Iterador1), AbsIt2 is abs(Iterador2), SumAbs is AbsIt1 + AbsIt2, SumAbs = 1.
%%En este ejercicio usamos Generate&Test, creando todos los posibles valores y evaluándolos más tarde con alguna restricción.

%% Ejercicio 4
%% vecinoLibre(+Pos, +Tablero, -PosVecino) idem vecino/3 pero además PosVecino
%% debe ser una celda transitable (no ocupada) en el Tablero
vecinoLibre(Pos,T,PosVecino) :- vecino(Pos,T,PosVecino), posicion(PosVecino,T,_).


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
caminoConPosicionesVisitadas(F,F,_,_,[F]).
caminoConPosicionesVisitadas(I,F,T,L,[I|Cs]) :- vecinoLibre(I,T,V), not(member(V,L)), caminoConPosicionesVisitadas(V,F,T,[I|L],Cs).

%% sinRepetidos(+Ls)
sinRepetidos([]).
sinRepetidos([E|Ls]) :- not(member(E,Ls)), sinRepetidos(Ls).


%% Ejercicio 6
%% cantidadDeCaminos(+Inicio, +Fin, +Tablero, ?N) que indique la cantidad de caminos
%% posibles sin ciclos entre Inicio y Fin.
cantidadDeCaminos(_,_,_,_).

%% Ejercicio 7
%% camino2(+Inicio, +Fin, +Tablero, -Camino) ídem camino/4 pero se espera una heurística
%% que mejore las soluciones iniciales.
%% No se espera que la primera solución sea necesariamente la mejor.
%% Una solución es mejor mientras menos pasos se deba dar para llegar a
%% destino (distancia Manhattan). Por lo tanto, el predicado deberá devolver de a uno,
%% todos los caminos pero en orden creciente de longitud.
camino2(_,_,_,_).

%% Ejercicio 8
%% camino3(+Inicio, +Fin, +Tablero, -Camino) ídem camino2/4 pero se espera que
%% se reduzca drásticamente el espacio de búsqueda.
%% En el proceso de generar los potenciales caminos, se pueden ir sacando algunas conclusiones.
%% Por ejemplo, si se está en la celda (3,4) y se dieron ya 6 pasos desde el Inicio,
%% entonces no tiene sentido seguir evaluando cualquier camino que implique llegar a la celda (3,4)
%% desde Inicio en más de 6 pasos.
%% Notar que dos ejecuciones de camino3/4 con los mismos argumentos deben dar los mismos resultados.
%% En este ejercicio se permiten el uso de predicados: dynamic/1, asserta/1, assertz/1 y retractall/1.
camino3(_,_,_,_).

%%%%%%%%%%%%%%%%%%%%%%%%
%% Tableros simultáneos
%%%%%%%%%%%%%%%%%%%%%%%%

%% Ejercicio 9
%% caminoDual(+Inicio, +Fin, +Tablero1, +Tablero2, -Camino) será verdadero
%% cuando Camino sea un camino desde Inicio hasta Fin pasando al mismo tiempo
%% sólo por celdas transitables de ambos tableros.
%% Nota: Es posible una implementación que resuelva en forma inmediata casos en los que trivialmente no existe camino dual posible.
caminoDual(_,_,_,_,_).
