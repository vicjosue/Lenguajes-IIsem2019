%========= Relaciones que definen el mecanismo gen�rico de resoluci�n =========

% La relaci�n test_puzzle/2 define un acertijo (puzzle/4) como un cu�druple que consiste de:
%   a) Structure: una estructura con los datos a determinar.
%   b) Clues: una lista de pistas que van instanciando los elementos de la estructura.
%   c) Queries: una lista de consultas a la estructura para responder a las preguntas.
%   d) Solution: una lista con las respuestas del acertijo.

test_puzzle(Name,puzzle(Structure,Clues,Queries,Solution)):-
   structure(Name,Structure),
   clues(Name,Structure,Clues),
   queries(Name,Structure,Queries,Solution).


% La relaci�n solve_puzzle/2 es el mecanismo por el cual se resuelve el acertijo.
% Simplemente se toma un acertijo con sus cuatro componentes. Va instanciando
% la estructura al ir resolviendo las pistas y las consultas.

solve_puzzle(puzzle(Structure, Clues,Queries,Solution),Solution):-
   solve(Clues),solve(Queries).


% La relaci�n solve/1 va tomando uno por uno los elementos de las listas
% ya sea de pistas o de consultas y los resuelve.
% N�tese c�mo solve toma el car de la lista de entrada y en la parte derecha
% usa ese car como meta a resolver.

solve([Clue|Clues]):-Clue,solve(Clues).
solve([]).


% Para mayor claridad se incluy� una relaci�n que imprime en
% diferentes l�neas las entradas de una lista.
% Se usar� para mostrar Structure.
mostrar([]).
mostrar([C|Cs]) :- writeln(C),mostrar(Cs).


% Esta es la relaci�n a llamar para resolver un acertijo.
% Ejemplo de uso:
%    ?- resolver(viajes, Struct, Sol).
%    Viaje(4,peck,protestas)
%    viaje(5,maddy,olimpiadas)
%    viaje(6,linda,eleccion)
%    viaje(7,tam,boda)
%    Struct = [viaje(4, peck, protestas), viaje(5, maddy, olimpiadas),
%              viaje(6, linda, eleccion), viaje(7, tam, boda)],
%    Sol = [['Las olimpiadas fueron cubiertas por ', maddy]]
%
% La l�nea "Struct = ..." se formateo para mayor legibilidad.

% resolver/3 recibe el nombre del acertijo y produce la soluci�n;
% se incluye Structure entre los t�rminos de resolver para que se pueda ver.
% Si no se incluye, Prolog lo usar� internamente pero no lo mostrar�.
% Eso har�a m�s dif�cil la depuraci�n.

resolver(Acertijo, Structure, Solucion) :-
         test_puzzle(Acertijo,Puzzle),    % Primero define el cu�druple usando el nombre.
         solve_puzzle(Puzzle,Solucion),   % Aplica las pistas y consultas y obtiene la soluci�n.
         Puzzle=puzzle(Structure,_,_,_),  % Se extrae la estructura del cu�druple para poder ver y depurar.
         mostrar(Structure),              % Muestra estructura en forma legible.
         mostrar(Solucion).

%========= Relaciones espec�ficas para un acertijo dado =========

% structure crea para un acertijo dado una lista con los elementos
% a determinar. Como no se ha resuelto a�n todas las partes de los
% elementos est�n sin instanciar:
%       viaje(fecha de Lanzamiento, candidatos, mision)

% A veces es posible instanciar una parte; en este caso es la posici�n ocupada.
% structure(NombreAcertijo, ListaConElementosADeterminar)

structure(viajes,[viaje(1,_,_),
                  viaje(2,_,_),
                  viaje(3,_,_),
                  viaje(4,_,_)]).


% Implementa las pistas dadas.
% Cada pista es un elemento de una lista.
% Cada una de ellas es una meta a resolver que va
% instanciando los elementos de la estructura.

clues(
      viajes,   % identifica las pistas como del acertijo "viajes"
      Viajes,   % la estructura del acertijo va atando todo

   [
     /* En lugar de invocar las pistas una por una como se muestra abajo,
        es mejor usar la relaci�n pistas/2; la cual recibe una lista con
        los n�meros de cada pista que se desea invocar.
        De esa manera se puede controlar m�s f�cilmente cu�les
        pistas se quieren probar y en qu� orden.
        Se tiene que cambiar las pistas de "pista1(Viajes)" a "pista(1,Viajes)".
     */
     pistas([5,3,2,1,4],Viajes)

     /*
     pista1(Viajes)  ,  % resuelve pista1 y va instanciando elementos de Viaje
     pista2(Viajes)  ,  % resuelve pista2 y va instanciando elementos de Viaje
     pista3(Viajes)  ,  % idem
     pista4(Viajes)  ,  % idem
     pista5(Viajes)     % idem
     */
   ]).

% Esta relaci�n hace consultas a la estructura com�n y luego
% prepara las respuestas del enunciado.
queries(
        viajes, % identifica las pistas como del acertijo "viajes"
        Viajes, % la estructura del acertijo va atando todo

        % Preguntas a la estructura
        [
          % Pregunta �Qui�n cubri� las olimipiadas?
          member(viaje(Mes,geraldine,Mision),Viajes)
        ],

        % Respuestas pedidas. Usa los valores determinados en la lista anterior.
        [
          ['Geraldine fue en el mes ', Mes, ' a la mision ', Mision]
        ]).

% Esta relaci�n invoca las pistas que se indican
% en la lista y EN EL ORDEN especificado en la lista.
pistas([],_).
pistas([P|Ps],E) :- pista(P,E), pistas(Ps,E).

% Implementaci�n de las pistas.
% Es mejor que cada pista sea un predicado aparte porque si se unen se debe
% evitar que haya variables comunes entre pistas. Toda conexi�n debe hacerse
% por medio de la estructura.


% 1.La persona asignada a la misi�n PR-97 se lanzar� en alg�n
% momento antes que la persona asignada a la misi�n CR-260.
pista(1,E) :- select(viaje(D2,_,cr-260),E,E2),
              select(viaje(D1,_,pr-97),E2,_),
              D1<D2.

% 2.	Del astronauta asignado a la misión TV-412 y la persona
% asignada a la misión CR-260, uno es Isaac y el otro se 
% lanzará en abril.
pista(2,E) :- member(viaje(_,isaac,tv-412),E),
              member(viaje(4,_,cr-260),E).

pista(2,E) :- member(viaje(4,_,tv-412),E),
              member(viaje(_,isaac,cr-260),E).

% 3.	La persona asignada a la misión TV-412 se lanzará
% 1 mes después de Francis.
pista(3,E) :- select(viaje(D1,francis,_),E,E2),
              select(viaje(D2,_,tv-412),E2,_),
              D1 is D2-1.

% 4.Patti se lanzará 2 meses 
% después del graduado asignado a la misión PR-97.
pista(4,E) :- select(viaje(D1,patti,_),E,E2),
              select(viaje(D2,_,pr-97),E2,_),
              D1 is D2+2.

% 5.	La persona que se lanza en enero es Isaac
% o el astronauta asignado a la misión AV-435.
pista(5,E) :- select(viaje(1,isaac,_),E,E2), member(viaje(_,_,av-435),E2);
              select(viaje(1,_,av-435),E,E2), member(viaje(_,isaac,_),E2).