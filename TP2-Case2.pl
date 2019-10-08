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
%       viaje(Orden,Nombre,Apertura,Movidas)

% A veces es posible instanciar una parte; en este caso es la posici�n ocupada.
% structure(NombreAcertijo, ListaConElementosADeterminar)

structure(viajes,[viaje(1,_,_,_),
                viaje(2,_,_,_),
                viaje(3,_,_,_),
                viaje(4,_,_,_),
                viaje(5,_,_,_),
                viaje(6,_,_,_),
                viaje(7,_,_,_)]).


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
    pistas([1,3,4,5,6,7,8,10,11,13,14,15,2,12,9],Viajes)

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
        member(viaje(_,Quien,eslava,_),Viajes), %16.¿Quién usó la apertura eslava?
        member(viaje(_,Quien2,_,34),Viajes), %17.¿Quién perdió en 34 movidas?
        member(viaje(_,sonia,A,_),Viajes) %18.¿Qué apertura usó Sonia?
        ],

        % Respuestas pedidas. Usa los valores determinados en la lista anterior.
        [
        ['la apertura eslava fue de',Quien],
        ['Quien perdio en 34 movidas fue',Quien2],
        ['la apertura que uso Sonia fue',A]
        ]).

% Esta relaci�n invoca las pistas que se indican
% en la lista y EN EL ORDEN especificado en la lista.
pistas([],_).
pistas([P|Ps],E) :- pista(P,E), pistas(Ps,E).

% Implementaci�n de las pistas.
% Es mejor que cada pista sea un predicado aparte porque si se unen se debe
% evitar que haya variables comunes entre pistas. Toda conexi�n debe hacerse
% por medio de la estructura.


% 1. La persona que usó la apertura Giuoco Piano
% jugó 1 juego después de Marta.
pista(1,E) :- select(viaje(O2,_,giuoco,_),E,E2),
              select(viaje(O1,marta,_,_),E2,_),
               O2 is O1+1.

% 2. Marta no perdió después de exactamente 25 movimientos.
pista(2,E) :- select(viaje(_,_,_,25),E,E2), select(viaje(_,marta,_,_),E2,_).

% 3. La persona que jugó de sexta, 
% la persona que comenzó con la Apertura Ware,
% Lucas, 
% la persona que comenzó con el Gambito Evans, 
% la persona que comenzó con el Gambito Benko 
%y el jugador que perdió en 29 movimientos fueron personas diferentes
pista(3,E) :- select(viaje(A,_,ware,_),E,E2),
            select(viaje(B,_,evans,_),E2,E3),
            select(viaje(C,_,benko,_),E3,E4),
            select(viaje(6,_,_,_),E4,E5),
            select(viaje(D,_,_,29),E5,_),
            A\==B,A\==C,A\==D,A\==6,
            B\==C,B\==D,B\==6,
            C\==D,C\==6,
            D\==6.

% 4. La persona que perdió en 48 movimientos no comenzó con el Ataque Torre.
pista(4,E) :- select(viaje(_,_,torre,_),E,E2),
              select(viaje(_,_,_,48),E2,_).

% 5. Víctor jugó 2 juegos antes de Vilma.
pista(5,E) :- select(viaje(O1,victor,_,_),E,E2),
              select(viaje(O2,vilma,_,_),E2,_),
              O2 is O1+2.

% 6. De la persona que jugó cuarto y el jugador que perdió en 26 movimientos, 
% uno fue Vilma y el otro comenzó con la Apertura Ware.
pista(6,E) :- select(viaje(4,vilma,_,_),E,E2),select(viaje(_,_,ware,26),E2,_);
              select(viaje(4,_,ware,_),E,E2),select(viaje(_,vilma,_,26),E2,_).

% 7. La persona que perdió en 18 movimientos no comenzó con el Gambito Evans.
pista(7,E) :- select(viaje(_,_,evans,_),E,E2),
              select(viaje(_,_,_,18),E2,_).

% 8. Lucas fue el jugador que comenzó con el Apertura Réti 
% o el jugador que comenzó con el Gambito Benko.
pista(8,E) :- member(viaje(_,lucas,reti,_),E);member(viaje(_,lucas,benko,_),E).

% 9. La persona que jugó séptimo no perdió después de exactamente 18 movimientos.
pista(9,E) :- select(viaje(_,_,_,18),E,E2),select(viaje(7,_,_,_),E2,_).

% 10. Irma jugó 1 juego después de la persona que comenzó con el Gambito Benko.
pista(10,E) :- select(viaje(O1,irma,_,_),E,E2),
               select(viaje(O2,_,benko,_),E2,_),
               O1 is O2+1.

% 11. La persona que comenzó con el Gambito Evans jugó 2 juegos después de Lucas.
pista(11,E) :- select(viaje(O1,_,evans,_),E,E2),
               select(viaje(O2,lucas,_,_),E2,_),
               O1 is O2+2.

% 12. La persona que perdió en 25 movimientos jugó  antes 
% que la persona que perdió en 30 movimientos.
pista(12,E) :- select(viaje(O1,_,_,25),E,E2),
               select(viaje(O2,_,_,30),E2,_),
               O1<O2.

% 13. Vilma no comenzó con el Ataque Torre.
pista(13,E) :- select(viaje(_,_,torre,_),E,E2),
               select(viaje(_,vilma,_,_),E2,_).

% 14. Rosa jugó 1 juego después de la persona que perdió en 30 movimientos.
pista(14,E) :- select(viaje(O1,rosa,_,_),E,E2),
               select(viaje(O2,_,_,30),E2,_),
               O1 is O2+1.

% 15. De la persona que perdió en 29 movimientos y Vilma,
% uno comenzó con Giuoco Piano y el otro jugó sexto.
pista(15,E) :- select(viaje(_,_,giuoco,29),E,E2),select(viaje(6,vilma,_,_),E2,_);
               select(viaje(6,_,_,29),E,E2),select(viaje(_,vilma,giuoco,_),E2,_).