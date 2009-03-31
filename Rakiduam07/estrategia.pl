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

:- module(estrategia, [estrategia/2,iniciar/1,get_team_behavior/1,obtener_posicion_rol/3], [assertions]).
%% modulo azul
%% módulo principal de la estrategia del equipo azul
%:- use_module(sim_video_server).
%% módulo de conexión con el servidor de video
%:- use_module(sim_command_server).
%% módulo de conexión con el servidor de comandos
:- use_module(primitivas).
:- use_module(navegacion).
%% módulo para gestión de logs
:- use_module(logger).
%% modulo para gestion de datos de ambiente
:- use_module(ambiente).
:- use_module(configuration).
:- use_module(evitacion).

:- comment(title, "Modulo estrategia").
:- comment(author, "Pablo Kogan").

:- comment(module, "Este módulo representa el el comportamiento de los agentes, la divicion de roles y todo lo que tenga que ver con la estrategia de juego. La interfaz esta establecida mediante el predicado estrategia.").

:- comment(doinclude,estrategia/2).


iniciar(Equipo):- iniciar_ambiente(Equipo).


:- pred estrategia(Estado,-ListaVelocidades) :: list * list 
 # "El predicado estrategia resuelve la acción a tomar (velocidad en cada rueda de cada robot @var{ListaVelocidades}) en base al @var{Estado} actual del ambiente.".
%% la función estrategia resuelve la acción a tomar (velocidad en cada rueda de cada robot) en base al Estado actual

estrategia(Estado,Vs):-
	%nl,display(Estado),nl,
	insertar_estado(Estado),
	get_team_behavior(Vs),
	%display('Vs:: '),display(Vs),nl,
	insertar_accion(Vs),
	!.


get_team_behavior(Vs) :-
	players_names(Players),
	get_behavior(Players,Vs),!.

get_behavior([],[]).

get_behavior([Name|Tail],[Vi,Vd|Vs]) :-
	comportamiento(Name,Vi,Vd),
	get_behavior(Tail,Vs).



% estrategia(Estado,[Iz1,De1,Iz2,De2,Iz3,De3,Iz4,De4,Iz5,De5]):-
% 	nl,display(Estado),nl,
% 	insertar_estado(Estado),
%  	comportamiento('kechu',Iz5,De5),
%  	%display(Iz5),display(','),display(De5),nl,
%  	comportamiento('meli',Iz4,De4),
%  	comportamiento('küla',Iz3,De3),
%  	comportamiento('epu',Iz2,De2),
%  	comportamiento('kiñe',Iz1,De1),
% 	display([Iz1,De1,Iz2,De2,Iz3,De3,Iz4,De4,Iz5,De5]),nl,
% 	insertar_accion([Iz1,De1,Iz2,De2,Iz3,De3,Iz4,De4,Iz5,De5]),
% 	%display(' paso insertar_accion'), 
% 	!.


%***************************asignación roles
% 
% asignacion_rol('kiñe','arquero').
% asignacion_rol('epu','jugador').
% asignacion_rol('küla','jugador').
% asignacion_rol('meli','jugador').
% asignacion_rol('kechu','jugador').
%****************************asignación robots
asignacion_robot(Name, robot(propio,Num,Pos)) :-
	get_player(Name,Num,propio),
	jugador(robot(propio,Num,Pos)).

asignacion_robot(_,_).
% asignacion_robot('kiñe',robot('propio',1,Pos)):-
% 	jugador(robot('propio',1,Pos)).
% asignacion_robot('epu',robot('propio',2,Pos)):-
% 	jugador(robot('propio',2,Pos)).
% asignacion_robot('küla',robot('propio',3,Pos)):-
% 	jugador(robot('propio',3,Pos)).
% asignacion_robot('meli',robot('propio',4,Pos)):-
% 	jugador(robot('propio',4,Pos)).
% asignacion_robot('kechu',robot('propio',5,Pos)):-
% 	jugador(robot('propio',5,Pos)).

%*******************************************comportamientos dinamicos*****************************************

% comportamiento(Jugador,Iz,De):-
% 	asignacion_rol(Jugador,Rol),
% 	asignacion_robot(Jugador,Robot),
% 	accion(Rol,Robot,Iz,De).

comportamiento(Jugador,Iz,De):-
	get_role(Jugador,Rol),
	asignacion_robot(Jugador,Robot),
	accion(Rol,Robot,Iz,De).
	%set_fact(behavior(Jugador,Iz,De)).

punto(1060,1445).


real_simulado(S):- get_environment(S).


distancia_al_arco(X):- real_simulado('real'), X is 200.
distancia_al_arco(X):- real_simulado('simulado'), X is 2.


linea_arquero(amarillo,Xinf):- 
   %arco_propio(Xinf,_Yinf,_Xsup,_Ysup), 
   cancha(Xinf,_Yinf,_Xsup,_Ysup).

linea_arquero(azul,Xsup):- 
   %arco_propio(Xinf,_Yinf,_Xsup,_Ysup), 
   cancha(_Xinf,_Yinf,Xsup,_Ysup).


zona_media1('azul',50.1219,6.3730,71.7739,77.2392):- real_simulado('simulado').
zona_media1('amarillo',28.46685,6.3730,50.1219,77.2392):- real_simulado('simulado').

zona_media1('azul',50.1219,6.3730,71.7739,77.2392):- real_simulado('real').
zona_media1('amarillo',28.46685,6.3730,50.1219,77.2392):- real_simulado('real').

zona_media2('azul',28.46685,6.3730,50.1219,77.2392):- real_simulado('simulado').
zona_media2('amarillo',50.1219,6.3730,71.7739,77.2392):- real_simulado('simulado').

zona_media2('azul',28.46685,6.3730,50.1219,77.2392):- real_simulado('real').
zona_media2('amarillo',50.1219,6.3730,71.7739,77.2392):- real_simulado('real').

zona_defensa('azul',71.7739,6.3730,93.4259,77.2392):- real_simulado('simulado').
zona_defensa('amarillo',6.8118,6.3730,28.46685,77.2392):- real_simulado('simulado').

zona_defensa('azul',71.7739,6.3730,93.4259,77.2392):- real_simulado('real').
zona_defensa('amarillo',6.8118,6.3730,28.46685,77.2392):- real_simulado('real').

zona_ataque('azul',6.8118,6.3730,28.46685,77.2392):- real_simulado('simulado').
zona_ataque('amarillo',71.7739,6.3730,93.4259,77.2392):- real_simulado('simulado'). 

zona_ataque('azul',6.8118,6.3730,28.46685,77.2392):- real_simulado('real').
zona_ataque('amarillo',71.7739,6.3730,93.4259,77.2392):- real_simulado('real').


pelota_en_zona_media1(E,X,Y):-
	zona_media1(E,Xi,Yi,Xs,Ys),
	%punto_intermedio esta definido en ambiente.pl
        punto_intermedio((Xi,Yi),(X,Y),(Xs,Ys)).

pelota_en_zona_media2(E,X,Y):-
	zona_media1(E,Xi,Yi,Xs,Ys),
	punto_intermedio((Xi,Yi),(X,Y),(Xs,Ys)).
	
pelota_en_zona_defensa(E,X,Y):-
	zona_defensa(E,Xi,Yi,Xs,Ys),
	punto_intermedio((Xi,Yi),(X,Y),(Xs,Ys)).

pelota_en_zona_ataque(E,X,Y):-
	zona_ataque(E,Xi,Yi,Xs,Ys),
	punto_intermedio((Xi,Yi),(X,Y),(Xs,Ys)).


en_campo_contrario(azul,X):-  medio_cancha(Xm,_Ym), X<Xm. 
en_campo_contrario(amarillo,X):- medio_cancha(Xm,_Ym), X>Xm.

en_campo_propio(amarillo,X):- medio_cancha(Xm,_Ym), X=<Xm. 

en_campo_propio(azul,X):- medio_cancha(Xm,_Ym), X>=Xm.


tirar_centro(E,X,Y):- real_simulado('simulado'), cerca_linea_fondo(E,X), Y =< 20.
tirar_centro(E,X,Y):- real_simulado('simulado'), cerca_linea_fondo(E,X), Y >= 64.

tirar_centro(E,X,Y):- real_simulado('real'), 
	             cerca_linea_fondo(E,X),
		     ancho_cancha(A),Limite is A * 0.3, Y =< Limite.
	


tirar_centro(E,X,Y):- real_simulado('real'), 
	              cerca_linea_fondo(E,X), 
		      ancho_cancha(A),Limite is A * 0.3, Y >= (A- Limite).
		     


cerca_linea_fondo('azul',X):- real_simulado('simulado'), X =< 9.
cerca_linea_fondo('amarillo',X):- real_simulado('simulado'), X >= 90.


cerca_linea_fondo('azul',X):- real_simulado('real'), 
	               largo_cancha(L),Limite is L * 0.2,
		       X =< Limite.
	
cerca_linea_fondo('amarillo',X):- real_simulado('real'),
	               largo_cancha(L),Limite is L * 0.2,
		       X >= (L-Limite).
	








cercano_pelotaY(Yp,Yj):- real_simulado('simulado'), abs(Yp-Yj) >= 3, abs(Yp-Yj) =< 10.
%cercano_pelotaY(Yp,Yj):- real_simulado('real'), abs(Yp-Yj) >= 3, abs(Yp-Yj) =< 10.

pelota_adelante('azul',Xp,Xj):- Xj > Xp.
pelota_adelante('amarillo',Xp,Xj):- Xp > Xj.

supera('azul',X,Xj):- X >= Xj.
supera('amarillo',X,Xj):- X =< Xj.

retroceder('amarillo',-1).
retroceder('azul',1).

cte_recup(8):- real_simulado('simulado').
cte_recup(300):- real_simulado('real').

%-------------------------------------------------------------------------------------

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%% ACCIONES %%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%





%%%%%%%%%%%%%%%%%%%%%%%% ARQUERO %%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% % El arquero "sigue" el movimiento de la pelota cuando la misma
% % se encuentra en la franja horizontal que coincide con el arco.
% % accion('arquero',robot('propio',1,pos(Xj,Yj,Z,R)),Iz,De) :- equipo(E),
% % 	pelota_pred(_X,Y,_),
% %         arco_propio(_Xinf,Yinf,_Xsup,Ysup),
% % 	Y>Yinf, Y<Ysup, linea_arquero(E,Xrob),
% % 	%ir_a_posicion(robot('propio',1,pos(Xj,Yj,Z,R)),Xrob,Y,Iz,De).


% % La pelota se encuentra en un "Y" mayor que el "Y" superior del arco,
% % entonces el arquero se queda detenido en el extremo superior del arco.
% % accion('arquero',Robot,Iz,De) :- equipo(E),
% %          arco_propio(_Xinf,_Yinf,_Xsup,Ysup), 
% %          pelota_pred(_X,Y,_Z), Y >= Ysup, linea_arquero(E,Xrob), 
% %          ir_a_posicion(Robot,Xrob,Ysup,Iz,De).          

% % accion('arquero',Robot,Iz,De) :- equipo(E),
% %          arco_propio(_Xinf,Yinf,_Xsup,_Ysup), 
% %          pelota_pred(_X,Y,_Z), Y =< Yinf, linea_arquero(E,Xrob),
% %          ir_a_posicion(Robot,Xrob,Yinf,Iz,De).    	

% % accion(arquero,robot(propio,N,pos(Xr,Yr,Z,R)),Iz,De) :- %equipo(E),
% % 	2000 == Xr-400,
% % 	2000  Xr+400,
% % % 	Ya1 is Yr-400,
% % % 	Ya2 is Yr+400,

% %           %linea_arquero(E,Xrob),
% % 	  cancha(_Xi,_Yi,_Xs,Ys),
% % 	  medio_cancha(_Xmc,Ymc),
% % 	  Yj =< Ys - 300,
% % 	  Yj >= Ymc,
% %           display('Ir para arriba'),
% %           apuntar_ir_a_posicion(robot('propio',N,pos(Xj,Yj,Z,R)),2000,Ys,Iz,De).  


% accion('arquero',robot('propio',N,pos(Xj,Yj,Z,R)),Iz,De) :- %equipo(E),
%           %linea_arquero(E,Xrob),
% 	  cancha(_Xi,_Yi,_Xs,Ys),
% 	  medio_cancha(_Xmc,Ymc),
% 	  Yj =< Ys - 300,
% 	  Yj >= Ymc,
%           display('Ir para arriba'),
%           apuntar_ir_a_posicion(robot('propio',N,pos(Xj,Yj,Z,R)),2000,Ys,Iz,De).  

% accion('arquero',robot('propio',N,pos(Xj,Yj,Z,R)),Iz,De) :- % equipo(E),
%           %linea_arquero(E,Xrob),
% 	  cancha(_Xi,Yi,_Xs,_Ys), 
%           medio_cancha(_Xmc,Ymc),
% 	  Yj >= Yi + 300,
% 	  Yj =< Ymc,
%           display('Ir para abajo'),
%           apuntar_ir_a_posicion(robot('propio',N,pos(Xj,Yj,Z,R)),2000,Yi,Iz,De).  

% accion('arquero',Robot,Iz,De) :- %equipo(E),
%         % linea_arquero(E,Xrob),
% 	  medio_cancha(_Xmc,Ymc),
%           display('Volviendo al medio'),
%           apuntar_ir_a_posicion(Robot,2000,Ymc,Iz,De).  



% % accion('arquero2',robot('propio',N,pos(Xj,Yj,Z,R)),Iz,De) :-
% % 	  Xj>1800,
% % 	  Xj<2400,
% % 	  Y>600,
% % 	  Y<1200,
% % 	  pelota_pred(X,Y,_Z),
% % 	  apuntar_a_posicion(robot('propio',N,pos(Xj,Yj,Z,R)),X,Y,Iz,De).       


% accion('arquero2',robot('propio',N,pos(Xj,Yj,Z,R)),Iz,De) :-
%    pelota_pred(X,Y,_Z),
%  % display(X), display(','), display(Y),
%  % display('XXX'),display(Xj), display(','), display(Yj),nl,
%   % evitar(pos(N,Xj,Yj),pos(X,Y),pos(Xe,Ye)),
   %ir_a_posicion(robot('propio',N,pos(Xj,Yj,Z,R)),X,Y,Iz,De).       

%%%%%%%%%%%%%%%%%%%%%%%%%%%%VAMO' EQUIPO%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

obtener_posicion_rol(Rol, X,Y):-
	jugadores_propios(L),
	obtener_robot_rol(L,Rol,robot(_,_,pos(X,Y,_,_))).

%obtener_robot_rol()
obtener_robot_rol([Robot|_C], Rol,Robot):-
	asignacion_robot(Nombre, Robot),
	get_role(Nombre, Rol).

obtener_robot_rol([_Robot|C], Rol, V):-
	obtener_robot_rol(C, Rol, V).






cte_desplazamiento(azul,1000).
cte_desplazamiento(amarillo,-1000).

destino(azul,X,XN):- X >= XN.
destino(amarillo,X,XN):- X =< XN.

volver_area(azul,X,XN):- X =< XN.
volver_area(amarillo,X,XN):- X >= XN.

arquero_fuera_area(azul,Xj,X3):- Xj =< X3.
arquero_fuera_area(amarillo,Xj,X3):- Xj >= X3.	

cte_fuera_area(azul,Ancho_area,Cte):- Cte is (Ancho_area * (-1)).
cte_fuera_area(amarillo,Ancho_area,Cte):- Cte is Ancho_area.

posicion_arquero(azul,X):-
	cancha(_X1,_Y1,X2,_Y2),
	X is X2 - 200.
posicion_arquero(amarillo,X):-
	cancha(X1,_Y1,_X2,_Y2),
	X is X1 + 200.


pelota_area_contraria(X,Y):- 
         cancha(Xinf,_Yinf,_Xsup,_Ysup),
         medio_cancha(_Xm,Ym),
         ancho_area(Ancho),
         alto_area(Alto),
         Alto1 is (Alto/2)+Ym,
         Alto2 is (Alto/2)-Ym,
         PosX is Xinf + Ancho,
         Y >= Alto2, Y =< Alto1,
         X =< PosX.

pelota_area_propia(X,Y):- 
         cancha(_Xinf,_Yinf,Xsup,_Ysup),
         medio_cancha(_Xm,Ym),
         ancho_area(Ancho),
         alto_area(Alto),
         Alto1 is (Alto/2)+Ym,
         Alto2 is (Alto/2)-Ym,
         PosX is Xsup - (Ancho+200),
         Y >= Alto2, Y =< Alto1,
         X >= PosX.



% accion('arquero3',robot('propio',N,pos(Xj,Yj,Z,R)),Iz,De):-
%    pelota_pred(_X,Y,_Z),
   
%  %   arco_propio(Xinf,_Yinf,_Xsup,_Ysup),
   

%    ir_a_posicion(robot('propio',N,pos(Xj,Yj,Z,R)),Xinf-700,Y,Iz,De).        
  


% accion('arquero3',robot('propio',N,pos(Xj,Yj,Z,R)),Iz,De):-
% 	pelota_pred(X,Y,_Z),
% 	ir_a_posicion(robot('propio',N,pos(Xj,Yj,Z,R)),X,Y,Iz,De).




accion('arquero3',robot('propio',N,pos(Xj,Yj,Z,R)),Iz,De):-
    pelota_pred(X,Y,_Z),
    medio_cancha(Xm,Ym),
    arco_propio(Xi,_Yi,_Xs,_Ys),
    %X < Xm + 1000, 
    \+ pelota_area_propia(X,Y),
    X > Xm,
    ir_a_posicion(robot('propio',N,pos(Xj,Yj,Z,R)),Xi-200,Ym,Iz,De).


accion('arquero3',robot('propio',N,pos(Xj,Yj,Z,R)),Iz,De):-
    pelota_pred(X,Y,_Z),
     medio_cancha(Xm,_Ym),
    %pelota_area_propia(X,Y), 
    X > Xm,
    ir_a_posicion(robot('propio',N,pos(Xj,Yj,Z,R)),X,Y,Iz,De).


accion('arquero3',_Robot,0,0):-
    pelota_pred(X,_Y,_Z),
    medio_cancha(Xm,_Ym),
    %pelota_area_propia(X,Y),
    %arco_propio(Xi,_Yi,_Xs,_Ys),
    X =< Xm.




% La pelota supera los 3/4 cancha.  
accion('arquero2',robot('propio',N,pos(Xj,Yj,Z,R)),Iz,De) :-
	  equipo(E),
	  pelota_pred(X,_Y,_Z),
	  medio_cancha(Xm,Ym),
	  cte_desplazamiento(E,Valor),
	  Xm2 is Xm + Valor,
	  destino(E,X,Xm2),
          arco_contrario(Xi,_Yi,_Xs,_Ys),
	  llevar_pelota_a_posicion(robot('propio',N,pos(Xj,Yj,Z,R)),Xi,Ym,Iz,De).       

% La pelota esta lejos del arquero y el mismo esta fuera de su
% area, entonces vuelve al area.
accion('arquero2',robot('propio',N,pos(Xj,Yj,Z,R)),Iz,De) :-
	  equipo(E),
	  pelota_pred(X,_Y,_Z),
	  medio_cancha(Xm,Ym),
	  linea_arquero(E,Xarq),
          ancho_area(V),
	  cte_fuera_area(E,V,Cte),
          X3 is Xarq + Cte,
	  cte_desplazamiento(E,Valor),
          Xm2 is Xm + Valor,
	  volver_area(E,X,Xm2),
	  arquero_fuera_area(E,Xj,X3),
	  posicion_arquero(E,Xpos),
          ir_a_posicion(robot('propio',N,pos(Xj,Yj,Z,R)),Xpos,Ym,Iz,De).       

% La pelota esta lejos del arquero y el mismo esta en su area,
% entonces apunta hacia la pelota.
accion('arquero2',robot('propio',N,pos(Xj,Yj,Z,R)),Iz,De) :-
	  equipo(E),
	  pelota_pred(X,Y,_Z),
	  medio_cancha(Xm,_Ym),
	  cte_desplazamiento(E,Valor),
          Xm2 is Xm + Valor,
	  destino(E,X,Xm2),
          apuntar_a_posicion(robot('propio',N,pos(Xj,Yj,Z,R)),X,Y,Iz,De).       


% accion('arquero2',robot('propio',N,pos(Xj,Yj,Z,R)),Iz,De) :-
%   pelota_pred(X,Y,_Z),
%  medio_cancha(_Xmc,Ymc),
%  ir_a_posicion(robot('propio',N,pos(Xj,Yj,Z,R)),2000,Ymc,Iz,De).       
  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

accion('nada',_Robot,0,0).


accion('evitar',robot('propio',N,pos(Xj,Yj,Z,R)),Iz,De) :-
  	pelota_pred(X,Y,_Z),
       evitar(pos(N,Xj,Yj),pos(X,Y),pos(Xe,Ye)),
        
 	ir_a_posicion(robot('propio',N,pos(Xj,Yj,Z,R)),Xe,Ye,Iz,De).       



%la pelota supera al defensor y la misma esta en campo propio, el jugador rechaza
accion('defensor',robot('propio',N,pos(Xj,Yj,Z,R)),Iz,De) :-
  	pelota_pred(X,Y,_Z),
        medio_cancha(Xm,_Ym),
       
        X > Xm,
        X > Xj,
        
 	ir_a_posicion(robot('propio',N,pos(Xj,Yj,Z,R)),X+300,Y+300,Iz,De).       



accion('defensor',robot('propio',N,pos(Xj,Yj,Z,R)),Iz,De) :-
  	pelota_pred(X,Y,_Z),
        medio_cancha(Xm,_Ym),
       
        X > Xm,
        %X > Xj,
        
 	ir_a_posicion(robot('propio',N,pos(Xj,Yj,Z,R)),X,Y,Iz,De).       


accion('defensor',robot('propio',N,pos(Xj,Yj,Z,R)),Iz,De) :-
  	pelota_pred(X,_Y,_Z),
        medio_cancha(Xm,Ym),
       
        %X > Xm,
        %X > Xj,
        X =< Xm,
 	ir_a_posicion(robot('propio',N,pos(Xj,Yj,Z,R)),Xm,Ym,Iz,De).       


%accion('defensor',robot('propio',N,pos(Xj,Yj,Z,R)),Iz,De) :-
%  	pelota_pred(X,Y,_Z),
%        ir_a_posicion(robot('propio',N,pos(Xj,Yj,Z,R)),X,Y,Iz,De).       




% Tiene la pelota adelante (mirando arco contrario), en campo propio.
% El jugador va a posicion.

% La pelota esta en campo contrario, pero el jugador 4 esta mas cerca
% de la pelota, entonces el jugador 2 se va hacia el arco.
% accion('defensor',robot('propio',N,pos(Xj,Yj,Z,R)), Iz, De) :- 
%   	pelota_pred(Xp,_Yp,_Z), 
%         medio_cancha(Xm,Ym),
%         Xp=<Xm, 
%         jugador(robot('propio',3,P)),  %delantero es el 3
%         mas_cercano(robot('propio',3,P)), 
%         cancha(Xinf,_Yinf,_Xsup,_Ysup),
%         ancho_area_chica(A),
%         PosX is Xinf + A,
%         PosY is Ym,
%         ir_a_posicion(robot('propio',N,pos(Xj,Yj,Z,R)),PosX,PosY,Iz,De).



% accion('defensor',robot('propio',N,pos(Xj,Yj,Z,R)),Iz,De):-
%     pelota_pred(X,Y,_Z),
%     %medio_cancha(Xm,_Ym),
%     % X > Xm, X =< Xj,
%     ir_a_posicion(robot('propio',N,pos(Xj,Yj,Z,R)),X,Y,Iz,De).       


accion('delantero',robot('propio',N,pos(Xj,Yj,Z,R)),Iz,De) :-
  	pelota_pred(X,Y,_Z),
       % medio_cancha(Xm,_Ym),
       % X < Xm,
       % X > Xj,
        
 	ir_a_posicion(robot('propio',N,pos(Xj,Yj,Z,R)),X,Y,Iz,De).       



% %Atacando
% accion('delantero',robot('propio',N,pos(Xj,Yj,Z,R)),Iz,De):-
%   	pelota_pred(X,Y,_Z),
       
% 	medio_cancha(Xm,_Ym),  
%         X < Xm, 
%         X =< Xj,
%  	ir_a_posicion(robot('propio',N,pos(Xj,Yj,Z,R)),X,Y,Iz,De).       


% %Defendiendo
% accion('delantero',robot('propio',N,pos(Xj,Yj,Z,R)),Iz,De):-
%      pelota_pred(X,_Y,_Z),
%      medio_cancha(Xm,Ym),
%      %X >= Xj, %la pelota lo supera
%      X >= Xm, 
%      ir_a_posicion(robot('propio',N,pos(Xj,Yj,Z,R)),Xm,Ym,Iz,De).       
 

% % %esperando en mitad de cancha.(el defensor tiene la pelota para patear
% % accion('delantero',robot('propio',N,pos(Xj,Yj,Z,R)),Iz,De) :- 
% %  	pelota_pred(X,Y,_Z), 
% %         medio_cancha(Xm,_Ym),
% %         X >= Xm,
% %         jugador(robot('propio',1,pos(X4,_Y4,_,_))), %defensor es el 1
       
% %         X < X4, 
                 
% %        	ir_a_posicion(robot('propio',N,pos(Xj,Yj,Z,R)),Xm,Y+300,Iz ,De).


% % accion('delantero',robot('propio',N,pos(Xj,Yj,Z,R)),Iz,De) :- 
% %  	pelota_pred(X,_Y,_Z), 
% %         medio_cancha(Xm,_Ym),
% %         X > Xj,
% %         X >= Xm,  
% %         arco_propio(Xinf,_Yinf,_yXsup,Ysup), %esto corregirlo
% %  	ir_a_posicion(robot('propio',N,pos(Xj,Yj,Z,R)),Xinf-500,Ysup,Iz,De).


% % %siguiendo la pelota

% % accion('delantero',robot('propio',N,pos(Xj,Yj,Z,R)),Iz,De) :-
% %  	pelota_pred(X,Y,_Z), 
% %         ir_a_posicion(robot('propio',N,pos(Xj,Yj,Z,R)),X,Y,Iz ,De).







%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%VAMO' EQUIPO%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% % accion(prueba1,robot(propio,_,pos(Xr,Yr,_Zr,_R)),0,0) :-
% % %	Xa1 is Xr-300,
% % %	Xa2 is Xr+300,
% % %	Ya1 is Yr-300,
% % %	Ya2 is Yr+300,
% % %        pelota_entre(Xa1,Ya1,Xa2,Ya2),
% %  	punto(X,Y),
% % 	distancia((Xr,Yr),(X,Y),D),
% % 	D<250.


% % accion(prueba1,Robot,Iz,De) :-
% % %	pelota_pred(X,Y,_Z),
% % 	punto(X,Y),
% % 	ir_a_posicion(Robot,X,Y,Iz,De).

% accion(prueba1,Robot,Iz,De) :-
%         atascado(Robot),
% 	desbloquear(Robot,Iz,De).


% accion(prueba1,robot(propio,_,pos(Xr,Yr,_Zr,_R)),125,-125) :-
% % 	Xa1 is Xr-0,
% % 	Xa2 is Xr+100,
% % 	Ya1 is Yr-100,
% % 	Ya2 is Yr+100,
% 	pelota_pred(X,Y,_),
% 	distancia((Xr,Yr),(X,Y),D),
% 	D<50.


% accion(prueba1,Robot,Iz,De) :-
% 	pelota_pred(X,Y,_Z),
% 	apuntar_ir_a_posicion(Robot,X,Y,Iz,De).


% %%%%%%%%%%%%%%%%%%%%%55

accion(prueba,Robot,Iz,De) :-
        atascado(Robot),
	desbloquear(Robot,Iz,De).


accion(prueba,robot(propio,_,pos(Xr,Yr,_Zr,_R)),125,-125) :-
% 	Xa1 is Xr-0,
% 	Xa2 is Xr+100,
% 	Ya1 is Yr-100,
% 	Ya2 is Yr+100,
	pelota_pred(X,Y,_),
	distancia((Xr,Yr),(X,Y),D),
	D<50.


accion(prueba,Robot,Iz,De) :-
	pelota_pred(X,Y,_Z),
	ir_a_posicion(Robot,X,Y,Iz,De).


% accion(puntor,Robot,Iz,De) :-
% 	pelota_pred(X,Y,_Z),
% 	apuntar_a_posicion(Robot,X,Y,Iz,De).

% % accion(arquero,Robot,Iz,De):-
% % 	atajar(arquero,Robot,Iz,De).

% % El rol de jugador
% %si la pelota esta en el campo própio hace juego brusco
% accion(jugador,Robot,Iz,De):-
% 	campo_propio(X1c,Y1c,X2c,Y2c),	pelota_entre(X1c,Y1c,X2c,Y2c),
% 	brusco(Robot,Iz,De).

% %si la pelota esta en las bandas y el jugador esta entre los tres mas
% %cercanos hace juego brusco
% accion(jugador,Robot,Iz,De):-
% 	bandas(X1c,Y1c,X2c,Y2c),pelota_entre(X1c,Y1c,X2c,Y2c),
% 	mas_cercanos(Robot,3),
% 	%mas_cercanos(Robot,2),
% 	brusco(Robot,Iz,De).

% %si la pelota esta en el área contraria y el jugador esta entre los tres
% %mas cercanos realiza ataque_area
% accion(jugador,Robot,Iz,De):-
% 	area_contraria(Xa1,Ya1,Xa2,Ya2),
% 	pelota_entre(Xa1,Ya1,Xa2,Ya2),
% 	mas_cercanos(Robot,3),
% 	ataque_area(Robot,Iz,De).

% %si esta entre los tres más cercanos realiza ataque
% accion(jugador,Robot,Iz,De):-
% %	campo_contrario(X1c,Y1c,X2c,Y2c),
% %	pelota_entre(X1c,Y1c,X2c,Y2c),
% 	mas_cercanos(Robot,3),
% 	ataque(Robot,Iz,De).

% %si es el más cercano al centro de la cancha juega de pichero
% accion(jugador,Robot,Iz,De):-
% 	medio_cancha(X,Y),
% 	mas_cercanos_lugar(Robot,1,X,Y),
% 	pichero(Robot,Iz,De).

% %en caso que no se cumpla ninguna de las anteriores juega de líbero
% accion(jugador,Robot,Iz,De):-
% 	libero(Robot,Iz,De).



% % tratar el tema del not
% no(A):-A,!,fail.
% no(_).
% %****************************************
% %brusco es un comportamiento defensivo 
% brusco(Robot,Iz,De):-
% 	%si no soy el mas cercano y esta atascado entonces desbloquear
% 	(no(mas_cercano(Robot)),
%         atascado2(Robot);atascado(Robot)),!,
% 	desbloquear(Robot,Iz,De).

% brusco(Robot,Iz,De):-	
% 	%si esta en la zona del arquero entonces ir a la línea del área grande
% 	%para no hacer penal
%         area_chica_propia(Xa1,Ya1,Xa2,Ya2),
% 	robot_entre(Robot,Xa1-4,Ya1-4,Xa2+4,Ya2+4),
% 	pto_area(Robot,X,Y),
% 	ir_a_posicion_insegura(Robot,X,Y,Iz,De). 
	
% brusco(Robot,Iz,De):-	
% 	%si la pelota entro en el área grande ir al eje area mas cercano 
% 	%para no hacer penal
%         area_propia(Xa1,Ya1,Xa2,Ya2),
% 	pelota_entre(Xa1,Ya1,Xa2,Ya2),
% 	eje_area_mas_cercano(Robot,X,Y),
% 	ir_a_posicion_insegura(Robot,X,Y,Iz,De). 

% brusco(Robot,Iz,De):-  
% 	%si está detraz de la pelota ir a la pelota
% 	pelota_pred(Xb,Yb,_),
% 	sentido(S),
% 	cancha(X1,Y1,X2,Y2),
% 	(S>0, %el caso de azul-
% 	(Xb>90,X is Xb-S*2.5; X is Xb),
% 	robot_entre(Robot,X,Y1,X2,Y2);
% 	 S<0, %amarillo
% 	(Xb<9,X is Xb-S*2.5; X is Xb),
% 	robot_entre(Robot,X1,Y1,X,Y2)), 
% 	ir_a_posicion_insegura(Robot,Xb,Yb,Iz,De).

% brusco(Robot,Iz,De):-
% 	 % si no ir a posición defenciva
% 	centro_arco_propio(X,Y),
% 	acompannar_a_lleva_pelota_def(Robot,X,Y,Iz,De).


% %********************en ataque
% ataque_area(Robot,Iz,De):-
% 	%si no soy el mas cercano y esta atascado entonces desbloquear
% 	%(no(mas_cercano(Robot)),
%         atascado(Robot),
% 	desbloquear(Robot,Iz,De).
% ataque_area(Robot,Iz,De):-	
% 	%si esta en la zona del area chica contraria y no soy el mas cercano ir a la posición base
% 	area_chica_contraria(Xa1,Ya1,Xa2,Ya2),
% 	robot_entre(Robot,Xa1-2,Ya1-2,Xa2+2,Ya2+2),
% 	centro_arco_contrario(X,Y),
% 	(mas_cercano(Robot),
% 	 patear_pelota_a_posicion(Robot,X,Y,Iz,De);
% 	acompannar_a_lleva_pelota2(Robot,X,Y,Iz,De)).
% ataque_area(Robot,Iz,De):-
% 	%si el robot esta entre la pelota y el arco contrario
% 	%salir del lugar porque complica el gol
% 	centro_arco_contrario(X,Y),
% 	pelota_pred(Xb,Yb,_),
% 	sentido(S),
% 	robot_entre(Robot,Xb+S*3,Yb,X,Y),
% 	acompannar_a_lleva_pelota(Robot,X,Y,Iz,De).
% ataque_area(Robot,Iz,De):-
% 	%en caso contrario llevar pelota al arco contrario
% 	centro_arco_contrario(Xo,Yo),
% 	llevar_pelota_a_posicion(Robot,Xo,Yo,Iz,De).

% %**************************************************************	
% ataque(Robot,Iz,De):-
% 	%si no soy el mas cercano y esta atascado entonces desbloquear
% 	(no(mas_cercano(Robot)),
%         atascado2(Robot);atascado(Robot)),!,
% 	%display('atascado '),display(Robot),nl,
% 	desbloquear(Robot,Iz,De).

% ataque(Robot,Iz,De):-
% 	 % si esta entre la pelota y el arco contrario irse lejos
% 	%mas_cercano(Robot),
% 	centro_arco_contrario(X,Y),
% 	pelota_pred(Xb,Yb,_),
% 	sentido(S),
% 	robot_entre(Robot,Xb+S*3,Yb,X,Y),
% 	acompannar_a_lleva_pelota2(Robot,X,Y,Iz,De).

% ataque(Robot,Iz,De):-
% 	% si esta entre la pelota y el arco contrario irse lejos
% 	mas_cercanos2(Robot),
% 	centro_arco_contrario(Xo,Yo),
% 	llevar_pelota_a_posicion(Robot,Xo,Yo,Iz,De).
	
% ataque(Robot,Iz,De):-
% 	%en caso contrario ir hacia la pelota
%         pelota_pred(Xb,Yb,_),
% 	ir_a_posicion_insegura(Robot,Xb,Yb,Iz,De).
% 	%patear_pelota_a_posicion(Robot,X,Y,Iz,De).

% %**************************************************************
% libero(Robot,Iz,De):-
% 	%si no soy el mas cercano y esta atascado entonces desbloquear
% 	(no(mas_cercano(Robot)),
%         atascado2(Robot);atascado(Robot)),!,
% 	%display('atascado '),display(Robot),nl,
% 	desbloquear(Robot,Iz,De).

% libero(Robot,Iz,De):-
% 	%si esta en la zona del rol entonces llevar la pelota al objetivo
% 	 centro_arco_contrario(X,Y),
% 	 acompannar_a_lleva_pelota2(Robot,X,Y,Iz,De).

% %********************************************************************

% pichero(Robot,Iz,De):-
% 	%si no soy el mas cercano y esta atascado entonces desbloquear
% 	%(no(mas_cercano(Robot)),
%         atascado(Robot),
% 	desbloquear(Robot,Iz,De).
% %pichero(Robot,Iz,De):-
% 	%si esta en la zona del rol entonces llevar la pelota al objetivo
% 	%area_contraria(Xa1,Ya1,Xa2,Ya2),
% 	%pelota_entre(Xa1-2,Ya1+4,Xa2+2,Ya2-4),
% 	%mas_cercano(Robot),
% 	%centro_arco_contrario(X,Y),patear_pelota_a_posicion(Robot,X,Y,Iz,De).
% %pichero(Robot,Iz,De):-
% 	%si esta en la zona del rol entonces llevar la pelota al objetivo
% 	%area1('pichero',Xa1,Ya1,Xa2,Ya2),
% 	%pelota_entre(Xa1,Ya1,Xa2,Ya2),
% 	%centro_arco_contrario(X,Y),llevar_pelota_a_posicion(Robot,X,Y,Iz,De).
% pichero(Robot,Iz,De):-
% 	%si esta en dirección al area chica propia entonces esperar la pelota en el area
% 	area1('pichero2',Xa1,Ya1,Xa2,Ya2),
% 	pelota_en_direccion_a_zona(Xa1,Ya1,Xa2,Ya2,X,Y),
% 	(robot_entre(Robot,X-2,Y-2,X+2,Y+2),Iz is 0,De is 0;
% 	sentido(S),
% 	ir_a_posicion_precisa(Robot,X,Y+S*2,Iz,De)).

% pichero(Robot,Iz,De):-
% 	%caso contrario el comportamiento defecto es ir a la posición base del rol
% 	pelota_pred(Xb,_Yb,_),
% 	area1('pichero2',Xa1,Ya1,Xa2,Ya2),
% 	sentido(S),
% 	(Xb>Xa1,Xb<Xa2, X is Xb+S*2; Xb<Xa1, X is Xa1+S*2; X is Xa2+S*2),
% 	Y is (Ya2-Ya1)/2+Ya1,
% 	(robot_entre(Robot,X-2,Y-2,X+2,Y+2),Iz is 0,De is 0;
% 	ir_a_posicion_precisa(Robot,X,Y,Iz,De)). %posición baseposición base,

% %********************************************************************

% atajar(_Rol,Robot,Iz,De):-
% 	%si esta atascado entonces desbloquear
% 	atascado(Robot), %situación
% 	desbloquear(Robot,Iz,De). %comportamiento

% atajar(Rol,Robot,Iz,De):-
% 	%si esta en la zona del rol entonces llevar la pelota al objetivo
% 	area_propia(Xa1,Ya1,Xa2,Ya2),
% %	sentido(S),
% 	pelota_entre(Xa1,Ya1,Xa2,Ya2),
% 	mas_cercano(Robot),
% 	objetivo(Rol,X,Y),patear_pelota_a_posicion(Robot,X,Y,Iz,De).

% atajar(_Rol,Robot,Iz,De):-
% 	%si esta en dirección al area chica propia entonces esperar la pelota en el area
% 	area_chica_propia(Xa1,Ya1,Xa2,Ya2),
% 	pelota_en_direccion_a_zona(Xa1,Ya1,Xa2,Ya2-3,X,Y),
% 	(robot_entre(Robot,X-2,Y-2,X+2,Y+2),Iz is 0,De is 0;
% 	ir_a_posicion_precisa(Robot,X,Y,Iz,De)).

% atajar(Rol,Robot,Iz,De):-
% 	%caso contrario el comportamiento defecto es ir a la posición base del rol
% 	pos_base(Rol,X,Y),
% 	(robot_entre(Robot,X-2,Y-2,X+2,Y+2),Iz is 0,De is 0;
% 	ir_a_posicion_precisa(Robot,X,Y,Iz,De)). %po25sición baseposición base,


% %***********************************************************datos para estrategia
% area1('pichero2',12.7,39,87.5,44).
% area1('pichero',17,33,82,51).
% bandas(6,6,93.5,9).
% bandas(6,74.5,93.5,78).
% eje_area_mas_cercano(robot(_,_,pos(Xr,Yr,_,_)),X,Y):-
% 	sentido(S),
% 	S<0, %amarillo
% 	area_propia(_Xa1,Ya1,Xa2,Ya2),	
% 	(sqrt((Xa2-Xr)**2+(Ya1-Yr)**2)<sqrt((Xa2-Xr)**2+(Ya2-Yr)**2),
% 	Y is Ya1;Y is Ya2),
% 	X is Xa2.
% eje_area_mas_cercano(robot(_,_,pos(Xr,Yr,_,_)),X,Y):-
% 	sentido(S),S>0,
% 	area_propia(Xa1,Ya1,_Xa2,Ya2),	
% 	(sqrt((Xa1-Xr)**2+(Ya1-Yr)**2)<sqrt((Xa1-Xr)**2+(Ya2-Yr)**2),
% 	Y is Ya1;Y is Ya2),
% 	X is Xa1.
% %pto_area(robot(_,_,pos(_Xr,Y,_,_)),79,41).
% pto_area(robot(_,_,pos(_Xr,Yr,_,_)),X,Y):-
% 	sentido(S),
% 	S<0, %amarillo
% 	area_propia(_Xa1,_Ya1,Xa2,_Ya2),	
% 	Y is Yr+1,
% 	X is Xa2.
% %linea_area_mas_cercano(robot(_,_,pos(_Xr,Yr,_,_)),79.6,Yr).
% pto_area(robot(_,_,pos(_Xr,Yr,_,_)),X,Y):-
% 	sentido(S),
% 	S>0,
% 	area_propia(Xa1,_Ya1,_Xa2,_Ya2),	
% 	Y is Yr+1,
% 	X is Xa1.


% sentido(Signo):- %Signo =1 si es azul =-1 si es amarillo.
% 	largo_cancha(L),
% 	arco_propio(Xp1,_Yp1,_Xp2,_Yp2),
% 	arco_contrario(Xc1,_Yc1,_Xc2,_Yc2),
% 	Signo is (Xp1 - Xc1)/L.

% centro_arco_propio(X,Y):-
% 	arco_propio(X,Yp1,_Xp2,Yp2),
% 	Y is Yp1 + (Yp2-Yp1)/2.
% campo_propio(X1,Y1,X2,Y2):-
% 	largo_cancha(L),
% 	arco_propio(Xp1,_Yp1,_Xp2,_Yp2),
% 	arco_contrario(Xc1,_Yc1,_Xc2,_Yc2),
% 	Signo is (Xp1 - Xc1)/L,
% 	(Signo >0, %azul
% 	    cancha(_Xc1,Y1,X2,Y2),
% 	    medio_cancha(X1,_Y1);
% 	    Signo<0,
% 	    cancha(X1,Y1,_X2,Y2),
% 	    medio_cancha(X2,_Y1)).
% campo_contrario(X1,Y1,X2,Y2):-
% 	largo_cancha(L),
% 	arco_propio(Xp1,_Yp1,_Xp2,_Yp2),
% 	arco_contrario(Xc1,_Yc1,_Xc2,_Yc2),
% 	Signo is (Xp1 - Xc1)/L,
% 	(Signo <0, %amarillo
% 	    cancha(_Xc1,Y1,X2,Y2),
% 	    medio_cancha(X1,_Y1);
% 	    Signo>0,
% 	    cancha(X1,Y1,_X2,Y2),
% 	    medio_cancha(X2,_Y1)).
	 
% area_propia(X1,Y1,X2,Y2):-
% 	centro_arco_propio(X,Y),
% 	alto_area(A),
% 	Y1 is Y - A/2, %alto del area/2
% 	Y2 is Y + A/2,
% 	largo_cancha(L),
% 	arco_propio(Xp1,_Yp1,_Xp2,_Yp2),
% 	arco_contrario(Xc1,_Yc1,_Xc2,_Yc2),
% 	Signo is (Xp1 - Xc1)/L,
% 	ancho_area(An),
% 	Xmedio is X-Signo*An/2, %ancho del area/2
% 	X1 is Xmedio - An/2,
% 	X2 is Xmedio + An/2.

% area_chica_propia(X1,Y1,X2,Y2):-
% 	centro_arco_propio(X,Y),
% 	alto_area_chica(A),
% 	Y1 is Y - A/2, %alto del area/2
% 	Y2 is Y + A/2,
% 	largo_cancha(L),
% 	arco_propio(Xp1,_Yp1,_Xp2,_Yp2),
% 	arco_contrario(Xc1,_Yc1,_Xc2,_Yc2),
% 	Signo is (Xp1 - Xc1)/L,
% 	ancho_area_chica(An),
% 	Xmedio is X-Signo*An/2, %ancho del area/2
% 	X1 is Xmedio - An/2,
% 	X2 is Xmedio + An/2.
% %esta mal porque centro arco contrario es otro
% area_chica_contraria(X1,Y1,X2,Y2):-
% 	centro_arco_contrario(_X,Y),
% 	alto_area_chica(A),
% 	Y1 is Y - A/2, %alto del area/2
% 	Y2 is Y + A/2,
% 	largo_cancha(L),
% 	arco_propio(Xp1,_Yp1,_Xp2,_Yp2),
% 	arco_contrario(Xc1,_Yc1,_Xc2,_Yc2),
% 	Signo is (Xp1 - Xc1)/L,
% 	ancho_area_chica(An),
% 	Xmedio is Xc1+Signo*An/2, %ancho del area/2
% 	X1 is Xmedio - An/2,
% 	X2 is Xmedio + An/2.

% %ver error
% area_contraria(X1,Y1,X2,Y2):-
% 	centro_arco_contrario(_X,Y),
% 	alto_area(La),
% 	Y1 is Y - La/2, %alto del area/2
% 	Y2 is Y + La/2,
% 	largo_cancha(L),
% 	arco_propio(Xp1,_Yp1,_Xp2,_Yp2),
% 	arco_contrario(Xc1,_Yc1,_Xc2,_Yc2),
% 	Signo is (Xp1 - Xc1)/L,
% 	ancho_area(A),
% 	Xmedio is Xc1+(Signo*A/2), %ancho del area/2
% 	X1 is Xmedio - A/2,
% 	X2 is Xmedio + A/2.

% centro_arco_contrario(X,Y):-
%         largo_cancha(L),
% 	arco_propio(Xp1,_Yp1,_Xp2,_Yp2),
% 	arco_contrario(Xc1,Yc1,_Xc2,Yc2),
% 	Signo is (Xp1 - Xc1)/L,
% 	X is Xc1-Signo*2,
% 	Y is Yc1 + (Yc2-Yc1)/2.	
% %posiciones de los roles de la estrategia

% pos_base(arquero,X,Y):-
% 	pelota_pred(_Xb,Yb,_),
% 	area_chica_propia(X1,Y1,X2,Y2),
% 	largo_cancha(L),
% 	arco_propio(Xp1,_Yp1,_Xp2,_Yp2),
% 	arco_contrario(Xc1,_Yc1,_Xc2,_Yc2),
% 	Signo is (Xp1 - Xc1)/L,
%  	(Signo<0,X is X1; X is X2),
% 	(Y1<Yb,Yb<Y2,Y is Yb;
% 	Y1>Yb,Y is Y1;
% 	Y is Y2).
% pos_base(arquero,X,Y):-	centro_arco_propio(X,Y).
 
% pos_base('delantero_derecho',X,Y):-
% 	largo_cancha(L),
% 	ancho_cancha(A),
% 	medio_cancha(_Xmc,Ymc),
% 	arco_propio(Xp1,_Yp1,_Xp2,_Yp2),
% 	arco_contrario(Xc1,_Yc1,_Xc2,_Yc2),
% 	Signo is (Xp1 - Xc1)/L,
% 	X is Xc1 + Signo*L/4,
% 	Y is Ymc + Signo*A/4.
% pos_base('delantero_izquierdo',X,Y):-
% 	largo_cancha(L),
% 	ancho_cancha(A),
% 	medio_cancha(_Xmc,Ymc),
% 	arco_propio(Xp1,_Yp1,_Xp2,_Yp2),
% 	arco_contrario(Xc1,_Yc1,_Xc2,_Yc2),
% 	Signo is (Xp1 - Xc1)/L,
% 	X is Xc1 + Signo*L/4,
% 	Y is Ymc - Signo*A/4.
% pos_base('defensor_derecho',X,Y):-
% 	largo_cancha(L),
% 	ancho_cancha(A),
% 	medio_cancha(_Xmc,Ymc),
% 	arco_propio(Xp1,_Yp1,_Xp2,_Yp2),
% 	arco_contrario(Xc1,_Yc1,_Xc2,_Yc2),
% 	Signo is (Xp1 - Xc1)/L,
% 	X is Xp1 - Signo*L/4,
% 	Y is Ymc + Signo*A/4.
% pos_base('defensor_izquierdo',X,Y):-
% 	largo_cancha(L),
% 	ancho_cancha(A),
% 	medio_cancha(_Xmc,Ymc),
% 	arco_propio(Xp1,_Yp1,_Xp2,_Yp2),
% 	arco_contrario(Xc1,_Yc1,_Xc2,_Yc2),
% 	Signo is (Xp1 - Xc1)/L,
% 	X is Xp1 - Signo*L/4,
% 	Y is Ymc - Signo*A/4.
% area(arquero,X1,Y1,X2,Y2):-
% 	area_propia(X1,Y1,X2,Y2).
% area('delantero_derecho',X1,Y1,X2,Y2):-
% 	pos_base('delantero_derecho',X,Y),
% 	largo_cancha(L),
% 	ancho_cancha(A),
% 	X1 is X - L/4,
% 	Y1 is Y - A/4,
% 	X2 is X + L/4,
% 	Y2 is Y + A/4.
% area('delantero_izquierdo',X1,Y1,X2,Y2):-
% 	pos_base('delantero_izquierdo',X,Y),
% 	largo_cancha(L),
% 	ancho_cancha(A),
% 	X1 is X - L/4,
% 	Y1 is Y - A/4,
% 	X2 is X + L/4,
% 	Y2 is Y + A/4.
% area('defensor_derecho',X1,Y1,X2,Y2):-
% 	pos_base('defensor_derecho',X,Y),
% 	largo_cancha(L),
% 	ancho_cancha(A),
% 	X1 is X - L/4,
% 	Y1 is Y - A/4,
% 	X2 is X + L/4,
% 	Y2 is Y + A/4.
% area('defensor_izquierdo',X1,Y1,X2,Y2):-
% 	pos_base('defensor_izquierdo',X,Y),
% 	largo_cancha(L),
% 	ancho_cancha(A),
% 	X1 is X - L/4,
% 	Y1 is Y - A/4,
% 	X2 is X + L/4,
% 	Y2 is Y + A/4.
 
% objetivo(_Rol,X,Y):- 
% 	centro_arco_contrario(X,Y).

   