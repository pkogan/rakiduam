%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%     Copyright 2007 Pablo Kogan, Guillermo Torres, Mario Moya
%
%     This file is part of Rakiduam.
%
%     Rakiduam is free software; you can redistribute it and/or modify
%     it under the terms of the GNU General Public License as published by
%     the Free Software Foundation; either version 3 of the License, or
%     (at your option) any later version.
%
%     Rakiduam is distributed in the hope that it will be useful,
%     but WITHOUT ANY WARRANTY; without even the implied warranty of
%     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%     GNU General Public License for more details.
%
%     You should have received a copy of the GNU General Public License
%     along with this program.  If not, see <http://www.gnu.org/licenses/>.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(estrategia, [estrategia/2,iniciar/1], [assertions]).
%% modulo azul
%% módulo principal de la estrategia del equipo azul
%:- use_module(sim_video_server).
%% módulo de conexión con el servidor de video
%:- use_module(sim_command_server).
%% módulo de conexión con el servidor de comandos
:- use_module(primitivas).
%% módulo para gestión de logs
:- use_module(logger).
%% modulo para gestion de datos de ambiente
:- use_module(ambiente).
:- comment(title, "Modulo estrategia").
:- comment(author, "Pablo Kogan").

:- comment(module, "Este módulo representa el el comportamiento de los agentes, la divicion de roles y todo lo que tenga que ver con la estrategia de juego. La interfaz esta establecida mediante el predicado estrategia.").

:- comment(doinclude,estrategia/2).

%arco_propio(93.4259,33.9320,93.4259,49.6801). arco_contrario(6.8118,33.9320,6.8118,49.6801).
%arco amarillo
%arco_contrario(93.4259,33.9320,93.4259,49.6801). arco_propio(6.8118,33.9320,6.8118,49.6801).

iniciar(Equipo):- iniciar_ambiente(Equipo).


:- pred estrategia(Estado,-ListaVelocidades) :: list * list 
 # "El predicado estrategia resuelve la acción a tomar (velocidad en cada rueda de cada robot @var{ListaVelocidades}) en base al @var{Estado} actual del ambiente.".
%% la función estrategia resuelve la acción a tomar (velocidad en cada rueda de cada robot) en base al Estado actual

%estrategia(Estado,[0,0,0,0,0,0,0,0,125,-125]).


estrategia(Estado,[Iz1,De1,Iz2,De2,Iz3,De3,Iz4,De4,Iz5,De5]):-
	%nl,display(Estado),nl,
	insertar_estado(Estado),
 	comportamiento('kechu',Iz5,De5),
 	%display(Iz5),display(','),display(De5),nl,
 	comportamiento('meli',Iz4,De4),
 	comportamiento('küla',Iz3,De3),
 	comportamiento('epu',Iz2,De2),
	comportamiento('kiñe',Iz1,De1),
	insertar_accion([Iz1,De1,Iz2,De2,Iz3,De3,Iz4,De4,Iz5,De5]),
	%display(' paso insertar_accion'), 
	!.


%***************************asignación roles
asignacion_rol('kiñe','arquero').
asignacion_rol('epu','jugador').
asignacion_rol('küla','nada').
asignacion_rol('meli','nada').
asignacion_rol('kechu','nada').
%****************************asignación robots
asignacion_robot('kiñe',robot('propio',1,Pos)):-
	jugador(robot('propio',1,Pos)).
asignacion_robot('epu',robot('propio',2,Pos)):-
	jugador(robot('propio',2,Pos)).
asignacion_robot('küla',robot('propio',3,Pos)):-
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

accion('nada',_,0,0).

accion('arquero',Robot,Vi,Vd):-
	arco_propio(X1,Y1,_X2,Y2),
	pelota_pred(_Xp,Yp,_),
	Yp=<Y2, Yp>=Y1,
	ir_a_posicion_y_apuntar(Robot,X1,Yp,90,Vi,Vd).
accion('arquero',Robot,Vi,Vd):-
	arco_propio(X1,_Y1,_X2,Y2),
	pelota_pred(_Xp,Yp,_),
	Yp>Y2,
	ir_a_posicion_y_apuntar(Robot,X1,Y2,90,Vi,Vd).	
accion('arquero',Robot,Vi,Vd):-
	arco_propio(X1,Y1,_X2,_Y2),
	pelota_pred(_Xp,Yp,_),
	Yp<Y1,
	ir_a_posicion_y_apuntar(Robot,X1,Y1,90,Vi,Vd).	


% accion('jugador',Robot,Iz,De) :-
%   	pelota_pred(X,Y,_),
% 	pelota(Xball,Yball,_),
% 	display('pelota predecida: '), display(X),display(' '),display(Y), nl,
% 	display('pelota: '), display(Xball), display(' '), display(Yball), nl,
% 	%robot('propio',Num,pos(A,B,C,Angle)) is Robot,
% 	%display('robot: '), display( Num), display(' : '), display(A), display(' '), display(B), display(' '), 
% 	%display(Angle), nl,
%   	ir_a_posicion(Robot,X,Y,Iz,De).
%  	%avanzar(robot('propio',2,Pos),Iz,De).


accion('pateador',robot('propio',Num,pos(A,B,_C,Angle)),Iz,De):-
	tiene_pelota(robot('propio',Num,pos(A,B,_C,Angle))),
	A >= 7,
	A =< 25,
	B >= 29, 
	B =< 55,
 	patear(robot('propio',Num,pos(A,B,_C,Angle)), 'to_right', 100, _Angulo, Iz,De).

accion('pateador',robot('propio',Num,pos(A,B,_C,Angle)),Iz,De) :-
	accion_prev(Num,Vi,Vd),
	agregar_comentario(Vi),agregar_comentario(Vd),
  	ir_a_posicion_y_apuntar(robot('propio',Num,pos(A,B,_C,Angle)),45,20,30,Iz,De).

	
accion('jugador',Robot,Iz,De):- 
	llevar_pelota_a_posicion(Robot,6,42,Iz,De).
	%patear(robot('propio',_Num,pos(A,B,_C,Angle)), 'to_right', 100, _Angulo, Iz,De).
%	pelota_pred(X,Y,_),
%	ir_a_posicion(Robot,X,Y,Iz,De).


%accion('jugador',robot('propio',Num,pos(A,B,_C,Angle)),Iz,De) :-
%  	pelota_pred(X,Y,_),
%	pelota(Xball,Yball,_),
%	display('pelota predecida: '), display(X),display(' '),display(Y), nl,
%	display('pelota: '), display(Xball), display(' '), display(Yball), nl,
	%robot('propio',Num,pos(A,B,C,Angle)) is Robot,
%	display('robot: '), display(_Num), display(' : '), display(A), display(' '), display(B), display(' '), 
%	display(Angle), nl,
%  	ir_a_posicion(robot('propio',Num,pos(A,B,_C,Angle)),X,Y,Iz,De).
 	%avanzar(robot('propio',2,Pos),Iz,De).



accion('jugadorgira',_R,Vi,Vd):-	
%	ir_a_posicion_y_apuntar(R,21,43,90,Vi,Vd).
	gira(Vi,Vd,50,'to_right',50).

accion('jugadorapunta',Robot,Vi,Vd):-
	medio_cancha(X,Y),
	pelota_pred(Xp,Yp,_),
	calcular_angulo(Robot,Xp,Yp,Angulo),
	ir_a_posicion_y_apuntar(Robot,X,Y,Angulo,Vi,Vd).
