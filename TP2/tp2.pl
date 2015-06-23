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
%% posibles sin ciclos entre Inicio y Fin. La idea basica es la misma que contar la cantidad de 
%% hojas en un arbol binario. El caso base se encuentra cuando Inicio es igual a Fin, en donde N se 
%% define en 1; y el caso recursivo es explorar cada uno de los vecinos del casillero actual, 
%% definiendo a N como la suma de sus resultados.
cantidadDeCaminos(I,I,_,1).
cantidadDeCaminos(I,F,T,N) :- expandir(I,F,T,[I],N).

%% expandir(+Inicio, +Fin, +Tablero, +Historial, ?N) expande la exploracion de caminos a las
%% cuatro posiciones aldañas posibles. Define a N como la suma de los resultados de la 
%% exploracion de las mismas.
expandir(pos(X,Y),F,T,H,N) :- I=pos(X,Y), R is Y+1, L is Y-1, D is X+1, U is X-1, explorar(I,pos(X,R),F,T,H,N1), 
	explorar(I,pos(X,L),F,T,H,N2), explorar(I,pos(U,Y),F,T,H,N3), explorar(I,pos(D,Y),F,T,H,N4), N is N1 + N2 + N3 + N4.

%% explorar(+Anterior, +Inicio, +Fin, +Tablero, +Historial, ?N) explora todos los caminos sin 
%% ciclos entre Inicio y Fin fijando a N como la cantidad de caminos posibles, teniendo en cuenta que
%% Anterior es el casillero desde el que se expande la exploracion. En el caso que el Inicio y el Fin 
%% sean iguales la cantidad de caminos posibles es 1. En el caso de que el casillero Inicial ya haya 
%% sido tocado (se encuentra en historial) o no sea un vecino valido del anterior, N se fija en 0.
explorar(_,I,I,_,_,1).
explorar(A,I,F,T,_,N) :- I\=F, not(vecinoLibre(A,T,I)), N=0.
explorar(_,I,F,_,H,N) :- I\=F, member(I,H), N=0.
explorar(A,I,F,T,H,N) :- I\=F, not(member(I,H)), vecinoLibre(A,T,I), NH=[I|H], expandir(I,F,T,NH,N).


%% Ejercicio 7
%% camino2(+Inicio, +Fin, +Tablero, -Camino) ídem camino/4 pero se espera una heurística
%% que mejore las soluciones iniciales.
%% No se espera que la primera solución sea necesariamente la mejor.
%% Una solución es mejor mientras menos pasos se deba dar para llegar a
%% destino (distancia Manhattan). Por lo tanto, el predicado deberá devolver de a uno,
%% todos los caminos pero en orden creciente de longitud.
camino2(Inicio,Fin,T,C):- camino2SinCiclos(Inicio,Fin,T,[],C).

% camino2SinCiclos(+Inicio, +Fin, +Tablero, +Recorridos, -Camino)
% Las reglas definen 1 paso. En cada caso priorizo avanzar hacia la celda Fin.
% Los casos posibles son:
%        Base: Ya llegué a destino.
%        Inicio y Fin están en la misma fila.
%        Inicio y Fin están en la misma columna.
%        Ninguno de los anteriores.

% Base (llegué a destino):
camino2SinCiclos(Fin, Fin, _, _, [Fin]).

%% Si Inicio y Fin están en la misma columna, avanzar verticalmente si es posible y si no moverse horizontalmente
%% hacia cualquier costado (porque se alejaría y da lo mismo cualquier costado). Y si no puede tampoco, entonces retroceder
%% verticalmente (y se agotaron las opciones).
camino2SinCiclos(pos(I1,J), pos(I2,J), T, Recorridos, [pos(I1,J)|[V|Demas]]):- 
		I1 =\= I2,
		Inicio = pos(I1,J), Fin = pos(I2,J),    % Renombres
		vecinoLibre(Inicio,T,pos(Iv,J)),        % solo vecinos de la misma columna (avance vertical)
		Iv =:= I1+sign(I2-I1),                  % Dar un paso vertical avanzando hacia Fin.
		V = pos(Iv,J),                          % Renombres
		not(member(V,Recorridos)),              % Sin ciclos
		camino2SinCiclos(V,Fin,T,[Inicio|Recorridos],[V|Demas]).

camino2SinCiclos(pos(I1,J), pos(I2,J), T, Recorridos, [pos(I1,J)|[V|Demas]]):- 
		I1 =\= I2,
		Inicio = pos(I1,J), Fin = pos(I2,J),    % Renombres
		vecinoLibre(Inicio,T,pos(I1,Jv)),       % solo vecinos de la misma fila que Inicio (avance horizontal)
		V = pos(I1,Jv),                         % Renombres
		not(member(V,Recorridos)),              % Sin ciclos
		camino2SinCiclos(V,Fin,T,[Inicio|Recorridos],[V|Demas]).
		
camino2SinCiclos(pos(I1,J), pos(I2,J), T, Recorridos, [pos(I1,J)|[V|Demas]]):- 
		I1 =\= I2,
		Inicio = pos(I1,J), Fin = pos(I2,J),    % Renombres
		vecinoLibre(Inicio,T,pos(Iv,J)),        % solo vecinos de la misma columna (retroceso vertical)
		Iv =:= I1-sign(I2-I1),                  % Dar un paso vertical avanzando hacia el otro lado.
		V = pos(Iv,J),                          % Renombres
		not(member(V,Recorridos)),              % Sin ciclos
		camino2SinCiclos(V,Fin,T,[Inicio|Recorridos],[V|Demas]).
		
%% Si Inicio y Fin están en la misma fila, avanzar horizontalmente si es posible y si no moverse verticalmente
%% hacia cualquier lado (porque se alejaría y da lo mismo cualquier lado). Y si no puede tampoco, entonces retroceder
%% horizontalmente (y se agotaron las opciones).
camino2SinCiclos(pos(I,J1), pos(I,J2), T, Recorridos, [pos(I,J1)|[V|Demas]]):- 
		J1 =\= J2,
		Inicio = pos(I,J1), Fin = pos(I,J2),    % Renombres
		vecinoLibre(Inicio,T,pos(I,Jv)),        % solo vecinos de la misma fila (avance horizontal)
		Jv =:= J1+sign(J2-J1),                  % Dar un paso horizontal avanzando hacia Fin.
		V = pos(I,Jv),                          % Renombres
		not(member(V,Recorridos)),              % Sin ciclos
		camino2SinCiclos(V,Fin,T,[Inicio|Recorridos],[V|Demas]).

camino2SinCiclos(pos(I,J1), pos(I,J2), T, Recorridos, [pos(I,J1)|[V|Demas]]):- 
		J1 =\= J2,
		Inicio = pos(I,J1), Fin = pos(I,J2),    % Renombres
		vecinoLibre(Inicio,T,pos(Iv,J1)),       % solo vecinos de la misma columna que Inicio (avance vertical)
		V = pos(Iv,J1),                         % Renombres
		not(member(V,Recorridos)),              % Sin ciclos
		camino2SinCiclos(V,Fin,T,[Inicio|Recorridos],[V|Demas]).
		
camino2SinCiclos(pos(I,J1), pos(I,J2), T, Recorridos, [pos(I,J1)|[V|Demas]]):- 
		J1 =\= J2,
		Inicio = pos(I,J1), Fin = pos(I,J2),    % Renombres
		vecinoLibre(Inicio,T,pos(I,Jv)),        % solo vecinos de la misma fila (retroceso horizontal)
		Jv =:= J1-sign(J2-J1),                  % Dar un paso horizontal avanzando hacia el otro lado.
		V = pos(I,Jv),                          % Renombres
		not(member(V,Recorridos)),              % Sin ciclos
		camino2SinCiclos(V,Fin,T,[Inicio|Recorridos],[V|Demas]).
		
%% Si Inicio y Fin NO están ni en la misma fila ni en la misma columna, entonces
%% intento avanzar primero verticalmente hacia Fin. Si no puedo, intento avanzar
%% horizontalmente hacia Fin. Si no puedo, retrocedo verticalmente. Y si no puedo, 
%% retrocedo horizontalmente. 
camino2SinCiclos(pos(I1,J1), pos(I2,J2), T, Recorridos, [pos(I1,J1)|[V|Demas]]):- 
		I1 =\= I2, J1 =\= J2,
		Inicio = pos(I1,J1), Fin = pos(I2,J2),    % Renombres
		vecinoLibre(Inicio,T,pos(Iv,J1)),         % solo vecinos de la misma columna (avance vertical)
		Iv =:= I1+sign(I2-I1),                    % Dar un paso vertical avanzando hacia Fin.
		V = pos(Iv,J1),                           % Renombres
		not(member(V,Recorridos)),                % Sin ciclos
		camino2SinCiclos(V,Fin,T,[Inicio|Recorridos],[V|Demas]).
		
camino2SinCiclos(pos(I1,J1), pos(I2,J2), T, Recorridos, [pos(I1,J1)|[V|Demas]]):-
		I1 =\= I2, J1 =\= J2,
		Inicio = pos(I1,J1), Fin = pos(I2,J2),    % Renombres
		vecinoLibre(Inicio,T,pos(I1,Jv)),         % solo vecinos de la misma fila (avance horizontal)
		Jv =:= J1+sign(J2-J1),                    % Dar un paso horizontal avanzando hacia Fin.
		V = pos(I1,Jv),                           % Renombres
		not(member(V,Recorridos)),                % Sin ciclos
		camino2SinCiclos(V,Fin,T,[Inicio|Recorridos],[V|Demas]).
		
camino2SinCiclos(pos(I1,J1), pos(I2,J2), T, Recorridos, [pos(I1,J1)|[V|Demas]]):- 
		I1 =\= I2, J1 =\= J2,
		Inicio = pos(I1,J1), Fin = pos(I2,J2),    % Renombres
		vecinoLibre(Inicio,T,pos(Iv,J1)),         % solo vecinos de la misma columna (retroceso vertical)
		Iv =:= I1-sign(I2-I1),                    % Dar un paso vertical retrocediendo.
		V = pos(Iv,J1),                           % Renombres
		not(member(V,Recorridos)),                % Sin ciclos
		camino2SinCiclos(V,Fin,T,[Inicio|Recorridos],[V|Demas]).
		
camino2SinCiclos(pos(I1,J1), pos(I2,J2), T, Recorridos, [pos(I1,J1)|[V|Demas]]):- 
		I1 =\= I2, J1 =\= J2,
		Inicio = pos(I1,J1), Fin = pos(I2,J2),    % Renombres
		vecinoLibre(Inicio,T,pos(I1,Jv)),         % solo vecinos de la misma fila (retroceso horizontal)
		Jv =:= J1-sign(J2-J1),                    % Dar un paso horizontal retrocediendo.
		V = pos(I1,Jv),                           % Renombres
		not(member(V,Recorridos)),                % Sin ciclos
		camino2SinCiclos(V,Fin,T,[Inicio|Recorridos],[V|Demas]).
		
		
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

% El algoritmo es goloso como en camino2, pero vamos guardando en la base
% de conocimiento las longitudes de los caminos que vamos obteniendo.
% De esta manera, luego de haber encontrado un primer camino (que por el
% algoritmo goloso se espera que sea más o menos bueno), se procede a recorrer 
% el espacio de búsqueda pero deteniendo la búsqueda cuando el camino ya resulta
% más largo que cualquiera de los caminos hallados.
% Cada camino nuevo tiene una longitud menor o igual al anterior, entonces
% la regla se agrega siempre arriba (para que Prolog mire sólo el primer caminoLongitud(X)
% y como es el más chico no necesita mirar otros).
camino3(Inicio,Fin,T,C):- 
		retractall(caminoLongitud(_)), camino3SinCiclos(Inicio,Fin,T,[],C).

% camino3SinCiclos(+Inicio, +Fin, +Tablero, +Recorridos, -Camino)
% Las reglas definen 1 paso. En cada caso priorizo avanzar hacia la celda Fin.
% Los casos posibles son:
%        Base: Ya llegué a destino.
%        Inicio y Fin están en la misma fila.
%        Inicio y Fin están en la misma columna.
%        Ninguno de los anteriores.
:- dynamic caminoLongitud/1.
caminoDeLongitudMenorA(X):- caminoLongitud(L), L < X, !.  % el cut obliga a que Prolog mire sólo el primer camino (el más corto).
noHayCaminoDeLongitudMenorA(X):- not(caminoDeLongitudMenorA(X)).

% Base (llegué a destino) (si entra en esta regla, es porque no hay un camino de longitud menor
% (lo checkea en la regla anterior, usando el +2)
camino3SinCiclos(Fin, Fin, _, Recorridos, [Fin]):-
		length(Recorridos, Longit),
		L is Longit+1,
		asserta(caminoLongitud(L)).

%% Si Inicio y Fin están en la misma columna, avanzar verticalmente si es posible y si no moverse horizontalmente
%% hacia cualquier costado (porque se alejaría y da lo mismo cualquier costado). Y si no puede tampoco, entonces retroceder
%% verticalmente (y se agotaron las opciones).
camino3SinCiclos(pos(I1,J), pos(I2,J), T, Recorridos, [pos(I1,J)|[V|Demas]]):-
		length(Recorridos, Longit),             % Los que recorrí antes del Inicio actual
		L is Longit + 2,                        % El camino que estoy armando tiene longitud Longit + 1 
		                                        % (Inicio y V seguro van a formar parte del camino resultante, ya que Inicio!=Fin)
		noHayCaminoDeLongitudMenorA(L),
		I1 =\= I2,
		Inicio = pos(I1,J), Fin = pos(I2,J),    % Renombres
		vecinoLibre(Inicio,T,pos(Iv,J)),        % solo vecinos de la misma columna (avance vertical)
		Iv =:= I1+sign(I2-I1),                  % Dar un paso vertical avanzando hacia Fin.
		V = pos(Iv,J),                          % Renombres
		not(member(V,Recorridos)),              % Sin ciclos
		camino3SinCiclos(V,Fin,T,[Inicio|Recorridos],[V|Demas]).

camino3SinCiclos(pos(I1,J), pos(I2,J), T, Recorridos, [pos(I1,J)|[V|Demas]]):- 
		length(Recorridos, Longit),
		L is Longit + 2,
		noHayCaminoDeLongitudMenorA(L),
		I1 =\= I2,
		Inicio = pos(I1,J), Fin = pos(I2,J),    % Renombres
		vecinoLibre(Inicio,T,pos(I1,Jv)),       % solo vecinos de la misma fila que Inicio (avance horizontal)
		V = pos(I1,Jv),                         % Renombres
		not(member(V,Recorridos)),              % Sin ciclos
		camino3SinCiclos(V,Fin,T,[Inicio|Recorridos],[V|Demas]).
		
camino3SinCiclos(pos(I1,J), pos(I2,J), T, Recorridos, [pos(I1,J)|[V|Demas]]):- 
		length(Recorridos, Longit),
		L is Longit + 2,
		noHayCaminoDeLongitudMenorA(L),
		I1 =\= I2,
		Inicio = pos(I1,J), Fin = pos(I2,J),    % Renombres
		vecinoLibre(Inicio,T,pos(Iv,J)),        % solo vecinos de la misma columna (retroceso vertical)
		Iv =:= I1-sign(I2-I1),                  % Dar un paso vertical avanzando hacia el otro lado.
		V = pos(Iv,J),                          % Renombres
		not(member(V,Recorridos)),              % Sin ciclos
		camino3SinCiclos(V,Fin,T,[Inicio|Recorridos],[V|Demas]).
		
%% Si Inicio y Fin están en la misma fila, avanzar horizontalmente si es posible y si no moverse verticalmente
%% hacia cualquier lado (porque se alejaría y da lo mismo cualquier lado). Y si no puede tampoco, entonces retroceder
%% horizontalmente (y se agotaron las opciones).
camino3SinCiclos(pos(I,J1), pos(I,J2), T, Recorridos, [pos(I,J1)|[V|Demas]]):- 
		length(Recorridos, Longit),
		L is Longit + 2,
		noHayCaminoDeLongitudMenorA(L),
		J1 =\= J2,
		Inicio = pos(I,J1), Fin = pos(I,J2),    % Renombres
		vecinoLibre(Inicio,T,pos(I,Jv)),        % solo vecinos de la misma fila (avance horizontal)
		Jv =:= J1+sign(J2-J1),                  % Dar un paso horizontal avanzando hacia Fin.
		V = pos(I,Jv),                          % Renombres
		not(member(V,Recorridos)),              % Sin ciclos
		camino3SinCiclos(V,Fin,T,[Inicio|Recorridos],[V|Demas]).

camino3SinCiclos(pos(I,J1), pos(I,J2), T, Recorridos, [pos(I,J1)|[V|Demas]]):- 
		length(Recorridos, Longit),
		L is Longit + 2,
		noHayCaminoDeLongitudMenorA(L),
		J1 =\= J2,
		Inicio = pos(I,J1), Fin = pos(I,J2),    % Renombres
		vecinoLibre(Inicio,T,pos(Iv,J1)),       % solo vecinos de la misma columna que Inicio (avance vertical)
		V = pos(Iv,J1),                         % Renombres
		not(member(V,Recorridos)),              % Sin ciclos
		camino3SinCiclos(V,Fin,T,[Inicio|Recorridos],[V|Demas]).
		
camino3SinCiclos(pos(I,J1), pos(I,J2), T, Recorridos, [pos(I,J1)|[V|Demas]]):- 
		length(Recorridos, Longit),
		L is Longit + 2,
		noHayCaminoDeLongitudMenorA(L),
		J1 =\= J2,
		Inicio = pos(I,J1), Fin = pos(I,J2),    % Renombres
		vecinoLibre(Inicio,T,pos(I,Jv)),        % solo vecinos de la misma fila (retroceso horizontal)
		Jv =:= J1-sign(J2-J1),                  % Dar un paso horizontal avanzando hacia el otro lado.
		V = pos(I,Jv),                          % Renombres
		not(member(V,Recorridos)),              % Sin ciclos
		camino3SinCiclos(V,Fin,T,[Inicio|Recorridos],[V|Demas]).
		
%% Si Inicio y Fin NO están ni en la misma fila ni en la misma columna, entonces
%% intento avanzar primero verticalmente hacia Fin. Si no puedo, intento avanzar
%% horizontalmente hacia Fin. Si no puedo, retrocedo verticalmente. Y si no puedo, 
%% retrocedo horizontalmente. 
camino3SinCiclos(pos(I1,J1), pos(I2,J2), T, Recorridos, [pos(I1,J1)|[V|Demas]]):- 
		length(Recorridos, Longit),
		L is Longit + 2,
		noHayCaminoDeLongitudMenorA(L),
		I1 =\= I2, J1 =\= J2,
		Inicio = pos(I1,J1), Fin = pos(I2,J2),    % Renombres
		vecinoLibre(Inicio,T,pos(Iv,J1)),         % solo vecinos de la misma columna (avance vertical)
		Iv =:= I1+sign(I2-I1),                    % Dar un paso vertical avanzando hacia Fin.
		V = pos(Iv,J1),                           % Renombres
		not(member(V,Recorridos)),                % Sin ciclos
		camino3SinCiclos(V,Fin,T,[Inicio|Recorridos],[V|Demas]).
		
camino3SinCiclos(pos(I1,J1), pos(I2,J2), T, Recorridos, [pos(I1,J1)|[V|Demas]]):-
		length(Recorridos, Longit),
		L is Longit + 2,
		noHayCaminoDeLongitudMenorA(L),
		I1 =\= I2, J1 =\= J2,
		Inicio = pos(I1,J1), Fin = pos(I2,J2),    % Renombres
		vecinoLibre(Inicio,T,pos(I1,Jv)),         % solo vecinos de la misma fila (avance horizontal)
		Jv =:= J1+sign(J2-J1),                    % Dar un paso horizontal avanzando hacia Fin.
		V = pos(I1,Jv),                           % Renombres
		not(member(V,Recorridos)),                % Sin ciclos
		camino3SinCiclos(V,Fin,T,[Inicio|Recorridos],[V|Demas]).
		
camino3SinCiclos(pos(I1,J1), pos(I2,J2), T, Recorridos, [pos(I1,J1)|[V|Demas]]):- 
		length(Recorridos, Longit),
		L is Longit + 2,
		noHayCaminoDeLongitudMenorA(L),
		I1 =\= I2, J1 =\= J2,
		Inicio = pos(I1,J1), Fin = pos(I2,J2),    % Renombres
		vecinoLibre(Inicio,T,pos(Iv,J1)),         % solo vecinos de la misma columna (retroceso vertical)
		Iv =:= I1-sign(I2-I1),                    % Dar un paso vertical retrocediendo.
		V = pos(Iv,J1),                           % Renombres
		not(member(V,Recorridos)),                % Sin ciclos
		camino3SinCiclos(V,Fin,T,[Inicio|Recorridos],[V|Demas]).
		
camino3SinCiclos(pos(I1,J1), pos(I2,J2), T, Recorridos, [pos(I1,J1)|[V|Demas]]):- 
		length(Recorridos, Longit),
		L is Longit + 2,
		noHayCaminoDeLongitudMenorA(L),
		I1 =\= I2, J1 =\= J2,
		Inicio = pos(I1,J1), Fin = pos(I2,J2),    % Renombres
		vecinoLibre(Inicio,T,pos(I1,Jv)),         % solo vecinos de la misma fila (retroceso horizontal)
		Jv =:= J1-sign(J2-J1),                    % Dar un paso horizontal retrocediendo.
		V = pos(I1,Jv),                           % Renombres
		not(member(V,Recorridos)),                % Sin ciclos
		camino3SinCiclos(V,Fin,T,[Inicio|Recorridos],[V|Demas]).


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
