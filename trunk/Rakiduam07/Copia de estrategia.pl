:- module(estrategia, [estrategia/2,iniciar/1], [assertions]).
%% modulo azul
%% m�dulo principal de la estrategia del equipo azul
%:- use_module(sim_video_server).
%% m�dulo de conexi�n con el servidor de video
%:- use_module(sim_command_server).
%% m�dulo de conexi�n con el servidor de comandos
:- use_module(primitivas).
%% m�dulo para gesti�n de logs
%:- use_module(logger).
%% modulo para gestion de datos de ambiente
:- use_module(ambiente).
:- comment(title, "Modulo estrategia").
:- comment(author, "Pablo Kogan").

:- comment(module, "Este m�dulo representa el el comportamiento de los agentes, la divicion de roles y todo lo que tenga que ver con la estrategia de juego. La interfaz esta establecida mediante el predicado estrategia.").

:- comment(doinclude,estrategia/2).

%arco_propio(93.4259,33.9320,93.4259,49.6801). arco_contrario(6.8118,33.9320,6.8118,49.6801).
%arco amarillo
%arco_contrario(93.4259,33.9320,93.4259,49.6801). arco_propio(6.8118,33.9320,6.8118,49.6801).

iniciar(Equipo):- iniciar_ambiente(Equipo).


:- pred estrategia(Estado,-ListaVelocidades) :: list * list 
 # "El predicado estrategia resuelve la acci�n a tomar (velocidad en cada rueda de cada robot @var{ListaVelocidades}) en base al @var{Estado} actual del ambiente.".
%% la funci�n estrategia resuelve la acci�n a tomar (velocidad en cada rueda de cada robot) en base al Estado actual

%estrategia(Estado,[0,0,0,0,0,0,0,0,125,-125]).


estrategia(Estado,[Iz1,De1,Iz2,De2,Iz3,De3,Iz4,De4,Iz5,De5]):-
	nl,display(Estado),nl,
	insertar_estado(Estado),
 	comportamiento('kechu',Iz5,De5),
 	%display(Iz5),display(','),display(De5),nl,
 	comportamiento('meli',Iz4,De4),
 	comportamiento('k�la',Iz3,De3),
 	comportamiento('epu',Iz2,De2),
	comportamiento('ki�e',Iz1,De1),
	insertar_accion([Iz1,De1,Iz2,De2,Iz3,De3,Iz4,De4,Iz5,De5]),
	%display(' paso insertar_accion'), 
	!.


%***************************asignaci�n roles
asignacion_rol('ki�e','arquero').
asignacion_rol('epu','jugador').
asignacion_rol('k�la','jugador').
asignacion_rol('meli','jugadorgira').
asignacion_rol('kechu','jugadorgira').
%****************************asignaci�n robots
asignacion_robot('ki�e',robot('propio',1,Pos)):-
	jugador(robot('propio',1,Pos)).
asignacion_robot('epu',robot('propio',2,Pos)):-
	jugador(robot('propio',2,Pos)).
asignacion_robot('k�la',robot('propio',3,Pos)):-
	jugador(robot('propio',3,Pos)).
asignacion_robot('meli',robot('propio',4,Pos)):-
	jugador(robot('propio',4,Pos)).
asignacion_robot('kechu',robot('propio',5,Pos)):-
	jugador(robot('propio',5,Pos)).

%*******************************************comportamientos dinamicos*****************************************

comportamiento(Jugador,Iz,De):-
	asignacion_rol(Jugador,Rol),
	asignacion_robot(Jugador,Robot),
	accion(Rol,Robot,Iz,De).

 

accion('arquero',_,0,125).


accion('jugador',Robot,Iz,De) :-
  	pelota_pred(X,Y,_),
  	ir_a_posicion(Robot,X,Y,Iz,De).
 	%avanzar(robot('propio',2,Pos),Iz,De).
	


accion('jugadorgira',_,Vi,Vd):-
	gira(Vi,Vd,50,'to_right',50).
