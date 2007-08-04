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

%:- module(primitivas, [patear_pelota_a_posicion/5,llevar_pelota_a_posicion/5,acompannar_a_lleva_pelota/5,acompannar_a_lleva_pelota2/5,acompannar_a_lleva_pelota_pres/5,acompannar_a_lleva_pelota_def/5,esperar_pelota_en_posicion/7,desbloquear/3,ir_a_posicion_segura/5,ir_a_posicion_segura_pres/5,ir_a_posicion_insegura/5,ir_a_posicion_precisa/5,ir_a_posicion/8,gira/5,goto_position/5,avanzar/3,distancia_entre_puntos/4], [assertions]).
:- module(primitivas,[ir_a_posicion/5,gira/5,resolver/4,patear/6,tiene_pelota/1,llevar_pelota_to_arco/3,ir_a_posicion_y_apuntar/6,calcular_angulo/4,llevar_pelota_a_posicion/5,diferencia_angular/3], [assertions]).
:- use_module(ambiente).
:- use_module(logger).

:- comment(title, "Modulo primitivas").

:- comment(author, "Pablo Kogan").

:- comment(module, "Este módulo representa las primitivas de movimiento de los agentes.").


:- comment(doinclude,ir_a_posicion/5).

llevar_pelota_a_posicion(robot(E,J,pos(Xr,Yr,_,Rr)),Xd,Yd,Vi,Vd):-
	%DVer is 5, %Ver DVer
	%DDest is 2.4, % Distancia a la pelota, desde el punto que se dirige el agente para llevar la pelota hacia el destino
	%Ver ángulo Diferencia angular 
	pelota_pred(Xball,Yball,_),
	Dy_Ball_Dest is Yd-Yball,
	Dx_Ball_Dest is Xd-Xball,
	Dy_Robot_Ball is Yball-Yr,
	Dx_Robot_Ball is Xball-Xr,
	Distancia is sqrt(Dx_Robot_Ball**2 + Dy_Robot_Ball**2),
	error_angular(Distancia,Error_Angular),
	%Error_Angular is 10,
	atan2(Dy_Ball_Dest,Dx_Ball_Dest,Ang_Ball_Dest),
	%display(Ang_Ball_Dest),nl,
	atan2(Dy_Robot_Ball,Dx_Robot_Ball,Ang_Robot_Ball),
	%display(Ang_Robot_Ball),
	diferencia_angular(Ang_Ball_Dest,Ang_Robot_Ball,Diferencia_Angular_Posicion),
	Ang_Dif is abs(Diferencia_Angular_Posicion),
	agregar_comentario('Distancia:'),
	agregar_comentario(Distancia),
	agregar_comentario(' por dif poscion = '),
	agregar_comentario(Ang_Dif),
	agregar_comentario('Angulos'),agregar_comentario(' '),
	agregar_comentario(Ang_Ball_Dest),agregar_comentario(' '),
	agregar_comentario(Ang_Robot_Ball),agregar_comentario(' '),
	agregar_comentario(Rr),agregar_comentario(' '),
	diferencia_angular(Ang_Ball_Dest,Rr,Diferencia_Angular_Angulo),
	Ang_Dif_Angulo is abs(Diferencia_Angular_Angulo),	
	agregar_comentario('por dif angulo= '),
	agregar_comentario(Ang_Dif_Angulo),
	Ang_Dif<Error_Angular,
	%Distancia<DVer,
	(Ang_Dif_Angulo<Error_Angular; (180-Ang_Dif_Angulo)< Error_Angular),
	agregar_comentario('ir_a_posicion por derecho'),
	ir_a_posicion(robot(E,J,pos(Xr,Yr,_,Rr)),Xball,Yball,Vi,Vd).


%pelota_pred2(5,5,5).
llevar_pelota_a_posicion(robot(E,J,pos(Xr,Yr,_,Rr)),Xd,Yd,Vi,Vd):-
	DVer is 3.7, %Ver DVer
	%DDest is 2.4, % Distancia a la pelota, desde el punto que se dirige el agente para llevar la pelota hacia el destino
	%Ver ángulo Diferencia angular 
	pelota_pred(Xball,Yball,_),
	Dy_Ball_Dest is Yd-Yball,
	Dx_Ball_Dest is Xd-Xball,
	Dy_Robot_Ball is Yball-Yr,
	Dx_Robot_Ball is Xball-Xr,
	Distancia is sqrt(Dx_Robot_Ball**2 + Dy_Robot_Ball**2),
	%error_angular(Distancia,Error_Angular),
	Error_Angular is 30,
	atan2(Dy_Ball_Dest,Dx_Ball_Dest,Ang_Ball_Dest),
	%display(Ang_Ball_Dest),nl,
	atan2(Dy_Robot_Ball,Dx_Robot_Ball,Ang_Robot_Ball),
	%display(Ang_Robot_Ball),
	diferencia_angular(Ang_Ball_Dest,Ang_Robot_Ball,Diferencia_Angular_Posicion),
	Ang_Dif is abs(Diferencia_Angular_Posicion),
	diferencia_angular(Ang_Ball_Dest,Rr,Diferencia_Angular_Angulo),
	Ang_Dif_Angulo is abs(Diferencia_Angular_Angulo),	
	%Ang_Dif<Error_Angular,
	Distancia<DVer,
	Ang_Dif<Error_Angular,
	(Ang_Dif_Angulo<Error_Angular; (180-Ang_Dif_Angulo)< Error_Angular),
	agregar_comentario('ir_a_posicion por cercano  '),
	ir_a_posicion(robot(E,J,pos(Xr,Yr,_,Rr)),Xball,Yball,Vi,Vd).
	
 llevar_pelota_a_posicion(robot(E,J,pos(Xr,Yr,_,Rr)),Xd,Yd,Vi,Vd):-
	DVer is 2.4, %Ver DVer
	%DDest is 2.4, % Distancia a la pelota, desde el punto que se dirige el agente para llevar la pelota hacia el destino
	%AVer is 30, %Ver ángulo Diferencia angular 
	pelota_pred(Xball,Yball,_),
	Dy_Ball_Dest is Yd-Yball,
	Dx_Ball_Dest is Xd-Xball,
	Dy_Robot_Ball is Yball-Yr,
	Dx_Robot_Ball is Xball-Xr,
	Distancia is sqrt(Dx_Robot_Ball**2 + Dy_Robot_Ball**2),
	error_angular(Distancia,Error_Angular),	
	%Error_Angular is 30,
	atan2(Dy_Ball_Dest,Dx_Ball_Dest,Ang_Ball_Dest),
	atan2(Dy_Robot_Ball,Dx_Robot_Ball,Ang_Robot_Ball),
	diferencia_angular(Ang_Ball_Dest,Ang_Robot_Ball,Diferencia_Angular),
	Ang_Dif is abs(Diferencia_Angular),
	%Ang_Dif>=Error_Angular,
	Ang_Pelota_Nuevo_Destino is Ang_Ball_Dest-180,
    	Dy=sin((3.14/180)*Ang_Pelota_Nuevo_Destino)*DVer,
	Dx=cos((3.14/180)*Ang_Pelota_Nuevo_Destino)*DVer,
	YNuevo is Yball+Dy,
	XNuevo is Xball+Dx,
	agregar_comentario('ir_a_posicion_y_apuntar'),	
	evacion_obstaculo(Xr,Yr,Xball,Yball,XNuevo,YNuevo,Xdv,Ydv,3.5),
	agregar_comentario(Xdv),agregar_comentario(' '),agregar_comentario(Ydv),
	ir_a_posicion_y_apuntar(robot(E,J,pos(Xr,Yr,_,Rr)),Xdv,Ydv,Ang_Robot_Ball,Vi,Vd).
	%ir_a_posicion(robot(E,J,pos(Xr,Yr,_,Rr)),Xdv,Ydv,Vi,Vd).
%************************************************************	
%evación del obstaculo Xo,Yo
%robot en posición X1,Y1
%destino inicial posición Xb,Yb

evacion_obstaculo(X1,Y1,Xo,Yo,Xb,Yb,Xb,Yb,Dist_Obst):-%no es obstaculo
	Xo<(X1-Dist_Obst),Xo<(Xb);
	Xo>(X1+Dist_Obst),Xo>(Xb);
	Yo<(Y1-Dist_Obst),Yo<(Yb);
	Yo>(Y1+Dist_Obst),Yo>(Yb).
evacion_obstaculo(X1,Y1,Xo,Yo,Xb,Yb,Xd,Yd,Dist_Obst):-
	%Cálculo Recta (Robot-Pelota) Yp = Mp*X + Cp 
	Dx is Xb-X1,
	Dy is Yb-Y1,
	%D_b is sqrt(Dx**2 + Dy**2), % pitagoras distancia al objetivo (Xb,Yb)
	Mp is Dy/Dx,
	Cp is Y1-Mp*X1,
	%Cálculo Recta perpendicular a Yp Yobs=Mobs*X + Cobs
	Mobs is -(1/Mp),
	Cobs is Yo-Mobs*Xo,
	%Cálculo del punto intersección de las rectas
	Xc is (Cobs-Cp)/(Mp-Mobs),
	Yc is Mp*Xc+Cp,
	%Cálculo de distancia del obstaculo a la intersección de las rectas
	D is sqrt((Xc-Xo)**2+(Yc-Yo)**2),
	(D<3.5, %Asumo 3.5 como la diagonal mayor del robot
		%Cálculo (Xs,Ys) (Xi,Yi) puntos alternativos para evadir el obstaculo
		% a Dt = 5 del obstaculo
		Raiz is sqrt(Dist_Obst**2 /(1+Mobs**2)), %5**2=25 Raiz es la diferencia en x
		Xs is Xo + Raiz,
		%display(Mp),		display(Mobs),
		Ys is Mobs*Xs + Cobs,
		Xi is Xo - Raiz,
		Yi is Mobs*Xi + Cobs,
		%display('Xs,Ys,Xi,Yi'),display(Xs),display(Ys),display(Xi),display(Yi),nl,
		%Se selecciona uno de los dos puntos dependiendo de la heuristica
		%Se toma el punto mas cercano
		Ds is sqrt((X1- Xs)**2 + (Y1- Ys)**2),
		Di is sqrt((X1- Xi)**2 + (Y1- Yi)**2),
		(Ds<Di,Xd is Xs,Yd is Ys;Xd is Xi,Yd is Yi);
		%else no es obstáculo
		Xd is Xb, Yd is Yb
	).
calcular_angulo(robot('propio',_Number,pos(Xr,Yr,_Z,_Rr)),Xp,Yp, Angulo) :-
	Dx is Xp-Xr,
	Dy is Yp-Yr,
	atan2(Dy,Dx,Angulo).

apuntar_a_posicion(robot('propio',_Number,pos(Xr,Yr,_Z,_Rr)),Xp,Yp, Vi,Vd) :-
	Dx is Xp-Xr,
	Dy is Yp-Yr,
	atan2(Dy,Dx,Angulo),
	apuntar(robot('propio',_Number,pos(Xr,Yr,_Z,_Rr)),Angulo,Vi,Vd).
	
	

ir_a_posicion_y_apuntar(robot('propio',Number,pos(Xr,Yr,_Z,_Rr)), X,Y,Angulo,Vi,Vd) :-
	Dx is X-Xr,
	Dy is Y-Yr,
	Distancia is sqrt(Dx**2 + Dy**2), % pitáagoras distancia al objetivo (X,Y)
	distancia_cero(Distancia),
	accion_prev(Number,0,0),
	agregar_comentario('Apuntando '),agregar_comentario(Number),
	apuntar(robot('propio',_Number,pos(Xr,Yr,_Z,_Rr)),Angulo,Vi,Vd).

ir_a_posicion_y_apuntar(robot('propio',Number,pos(Xr,Yr,_Z,_Rr)), X,Y,_Angulo,Vi,Vd) :-
	Dx is X-Xr,
	Dy is Y-Yr,
	Distancia is sqrt(Dx**2 + Dy**2), % pitáagoras distancia al objetivo (X,Y)
	distancia_cero(Distancia),
	\+(accion_prev(Number,0,0)),
	detener(Vi,Vd),
	agregar_comentario('Deteniendo '),agregar_comentario(Number).

ir_a_posicion_y_apuntar(robot('propio',_Number,pos(Xr,Yr,_Z,_Rr)), X,Y,_Angulo,Vi,Vd) :-
	Dx is X-Xr,
	Dy is Y-Yr,
	Distancia is sqrt(Dx**2 + Dy**2), % pitáagoras distancia al objetivo (X,Y)
	\+(distancia_cero(Distancia)),
	ir_a_posicion(robot('propio',_Number,pos(Xr,Yr,_Z,_Rr)),X,Y,Vi,Vd).

detener(0,0).			 
apuntar(robot('propio',_Number,pos(_Xr,_Yr,_Z,Rr)), Angulo,0,0) :-
	diferencia_angular(Angulo, Rr, Diferencia),	
	error_angular(0, Error),
	abs(Diferencia) =< Error.

apuntar(robot('propio',_Number,pos(_Xr,_Yr,_Z,Rr)), Angulo,Vi,Vd) :-
	diferencia_angular(Angulo,Rr, Diferencia),
	error_angular(0, Error),
	abs(Diferencia) > Error,	
	resolver_apuntar(Diferencia,0,Vi,Vd).

diferencia_angular(Alfa, Beta, Diferencia):-
	Aux is (Alfa - Beta),
	ajuste_angulo(Aux, Diferencia).

distancia_cero(Distancia):-
	Distancia =< (2).


ir_a_posicion(robot('propio',_Number,pos(Xr,Yr,_Z,_Rr)), X,Y,0,0) :-
	Dx is X-Xr,
	Dy is Y-Yr,
	Distancia is sqrt(Dx**2 + Dy**2), % pitáagoras distancia al objetivo (X,Y)
	distancia_cero(Distancia).

ir_a_posicion(robot('propio',Number,pos(Xr,Yr,_Z,Rr)), X,Y,Vi,Vd) :-
	Dx is X-Xr,
	Dy is Y-Yr,
	D_e is sqrt(Dx**2 + Dy**2), % pitáagoras distancia al objetivo (X,Y)
%	display('dx, dy, D_e'),nl,	display(Dx),nl,	display(Dy),nl,	display(D_e),nl,
	angulo_deseado(Dy,Dx,Rr,Theta_e),
%	display(' Theta_e '),	display(Theta_e),	nl,
	ajuste_angulo(Theta_e,Theta_e2),
%	display('Angulo ajustado '),	display(Theta_e2),	nl,
	resolver(Theta_e2,D_e,Vi,Vd),
	accion_prev(Number,ViPrev,VdPrev),
	Vi*ViPrev>=0,
	Vd*VdPrev>=0.
ir_a_posicion(robot('propio',Number,pos(Xr,Yr,_Z,Rr)), X,Y,Vi,Vd) :-
	Dx is X-Xr,
	Dy is Y-Yr,
	D_e is sqrt(Dx**2 + Dy**2), % pitáagoras distancia al objetivo (X,Y)
%	display('dx, dy, D_e'),nl,	display(Dx),nl,	display(Dy),nl,	display(D_e),nl,
	angulo_deseado(Dy,Dx,Rr,Theta_e),
%	display(' Theta_e '),	display(Theta_e),	nl,
	ajuste_angulo(Theta_e,Theta_e2),
%	display('Angulo ajustado '),	display(Theta_e2),	nl,
	resolver(Theta_e2,D_e,ViDes,VdDes),
	accion_prev(Number,ViPrev,VdPrev),
	(ViDes*ViPrev<0 ; VdDes*VdPrev<0),
	detener(Vi,Vd).	


avanzar(robot('propio',_Number,_Pos), Vi,Vd):-
	Vi is 125,
	Vd is 125.


	
% gira(125, -125, 100, 'to_right', 'inplace').
% gira(-125, 125, 100, 'to_left', 'inplace').
% gira(125, 0, 100, 'to_right', 'one_wheel').
% gira(0, 125, 100, 'to_left', 'one_wheel').

% gira(Vl,Vr, Fuerza_de_giro, 'to_right', 'inplace'):-
% 	Vl is Fuerza_de_giro*125/100,
% 	Vr is (Fuerza_de_giro*125/100)*(-1).

% gira(Vl,Vr, Fuerza_de_giro, 'to_left', 'inplace'):-
% 	Vl is (Fuerza_de_giro*125/100)*(-1),
% 	Vr is Fuerza_de_giro*125/100.

% gira(Vl,Vr, Fuerza_de_giro, 'to_right', 'one_wheel'):-
% 	Vl is Fuerza_de_giro*125/100,
% 	Vr is 0.

% gira(Vl,Vr, Fuerza_de_giro, 'to_left', 'one_wheel'):-
% 	Vl is 0,
% 	Vr is Fuerza_de_giro*125/100.

%Diferncial es re groso.
%to right implica que se aplica el diferencial sobre la rueda derecha

gira(Vl,Vr, Fuerza_de_giro, 'to_right', Diferencial):-
	Potencia is (Fuerza_de_giro*125/100),
	PotenciaDiferencial is ((Potencia * Diferencial)/100),
	Vr is  Potencia - PotenciaDiferencial,
	Vl is Potencia.
%to left implica que se aplica el diferencial sobre la rueda izquieda
gira(Vl,Vr, Fuerza_de_giro, 'to_left', Diferencial):-

	Potencia is (Fuerza_de_giro*125/100),
	PotenciaDiferencial is ((Potencia * Diferencial)/100),
	Vl is  Potencia - PotenciaDiferencial,
	Vr is Potencia.

% gira(Vl,Vr, Fuerza_de_giro, 'to_right', 'medium_arc'):-
% 	Vl is (Fuerza_de_giro-50)*125/100,
% 	Vr is Fuerza_de_giro*125/100.

% gira(Vl,Vr, Fuerza_de_giro, 'to_right', 'short_arc'):-
% 	Vl is (Fuerza_de_giro-80)*125/100,
% 	Vr is Fuerza_de_giro*125/100.



:- pred ir_a_posicion(+Xr,+Yr,+Rr,+X,+Y,+Vc,-Vi,-Vd) :: int * int * int * int * int * int * int * int 
 # "Este predicado devuelve la accion (@var{Vi}, @var{Vd}, potencia en las ruedas) necesaria para que el robot ubicado en el punto (@var{Xr},@var{Yr}) con una rotacion @var{Rr} y con una presición de @var{Vc}, se dirija al punto (@var{X},@var{Y})".

%Devuelve la velocidad en cada rueda Vi Vd para que el robot ubicado en Xr,Yr y rotación Rr
%se dirija a la posición X,Y destino

%error_angular(0, 3).
error_angular(_Distancia,7).

% error_angular(Distancia,Error) :-
% 	Distancia =< 5,
% 	Error is 45.
%
% error_angular(Distancia,Error) :-
% 	Distancia > 4,
% 	Error is 7.


%--------------------------------------
%voy para la izquierda
resolver_apuntar(Theta_e2,Distancia,Vi,Vd):-
	distancia_cero(Distancia),
	error_angular(Distancia,Error),
	Theta_e2>=Error , Theta_e2 < 90,
	PotenciaDeGiro is Theta_e2/2,
	Diferencial is 200,
	gira(Vi,Vd,PotenciaDeGiro,'to_left',Diferencial).

%voy para la derecha
resolver_apuntar(Theta_e2,Distancia,Vi,Vd):-
	distancia_cero(Distancia),
	error_angular(Distancia,Error),
	Theta_e2<(-Error) , Theta_e2 > (-90),
	PotenciaDeGiro is Theta_e2*(-1)/2,
	Diferencial is 200,
	gira(Vi,Vd,PotenciaDeGiro,'to_right',Diferencial).
	
resolver_apuntar(Theta_e2,Distancia,Vi,Vd):-
	distancia_cero(Distancia),
	Theta_e2=<(-90),
	PotenciaDeGiro is (Theta_e2+180)/2,
	Diferencial is 200,
	gira(Vi,Vd,PotenciaDeGiro,'to_right',Diferencial).

%voy para la derecha
resolver_apuntar(Theta_e2,Distancia,Vi,Vd):-
	distancia_cero(Distancia),
	Theta_e2>=90,
	PotenciaDeGiro is abs(Theta_e2-180)/2,
	Diferencial is 200,
	gira(Vi,Vd,PotenciaDeGiro,'to_left',Diferencial).
%-------------------------------------

calcular_potencia(_Distancia,100).
	
%	calcular_potencia(Distancia,10) :-
%		Distancia =< 4.2.
%	
%	calcular_potencia(Distancia,50) :-
%		Distancia =< 10.
%	
%	calcular_potencia(Distancia,100) :-
%		Distancia > 10.
	

%voy para adelante
resolver(Theta_e2,D_e,125,125):-
	error_angular(D_e,Error),
	Theta_e2>(-Error), Theta_e2<Error.
%voy para atras
resolver(Theta_e2,D_e,-125,-125):-
	error_angular(D_e,Error),
	(Theta_e2>(180-Error) ; Theta_e2<(-180+Error)).
%voy para la izquierda
resolver(Theta_e2,D_e,Vi,Vd):-
	error_angular(D_e,Error),
	Theta_e2>=Error , Theta_e2 < 75,
	calcular_potencia(D_e,PotenciaDeGiro),
	Diferencial is Theta_e2,
	gira(Vi,Vd,PotenciaDeGiro,'to_left',Diferencial).
resolver(Theta_e2,D_e,Vi,Vd):-
	error_angular(D_e,Error),
	Theta_e2>=75 , Theta_e2 < 90,
	calcular_potencia(D_e,PotenciaDeGiro),
	Diferencial is 200,
	gira(Vi,Vd,PotenciaDeGiro,'to_left',Diferencial).
	
%voy para la derecha
resolver(Theta_e2,D_e,Vi,Vd):-
	error_angular(D_e,Error),
	Theta_e2=<(-Error) , Theta_e2 > (-75),
	calcular_potencia(D_e,PotenciaDeGiro),
	Diferencial is Theta_e2*(-1),
	gira(Vi,Vd,PotenciaDeGiro,'to_right',Diferencial).

resolver(Theta_e2,D_e,Vi,Vd):-
	error_angular(D_e,Error),
	Theta_e2=<(-75) , Theta_e2 > (-90),
	calcular_potencia(D_e,PotenciaDeGiro),
	Diferencial is 200,
	gira(Vi,Vd,PotenciaDeGiro,'to_right',Diferencial).

	
	
resolver(Theta_e2,D_e,Vi,Vd):-
	Theta_e2=<(-105),
	calcular_potencia(D_e,Potencia),
	PotenciaDeGiro is Potencia*(-1),
	Diferencial is Theta_e2+180,
	gira(Vi,Vd,PotenciaDeGiro,'to_right',Diferencial).

resolver(Theta_e2,D_e,Vi,Vd):-
	Theta_e2>(-105),Theta_e2=<(-90),
	calcular_potencia(D_e,Potencia),
	PotenciaDeGiro is Potencia*(-1),
	Diferencial is 200,
	gira(Vi,Vd,PotenciaDeGiro,'to_right',Diferencial).
	

%voy para la derecha
resolver(Theta_e2,D_e,Vi,Vd):-
	Theta_e2>=105,
	calcular_potencia(D_e,Potencia),
	PotenciaDeGiro is Potencia*(-1),
	Diferencial is abs(Theta_e2-180),
	gira(Vi,Vd,PotenciaDeGiro,'to_left',Diferencial).

resolver(Theta_e2,D_e,Vi,Vd):-
	Theta_e2<(105),Theta_e2>=(90),
	calcular_potencia(D_e,Potencia),
	PotenciaDeGiro is Potencia*(-1),
	Diferencial is 200,
	gira(Vi,Vd,PotenciaDeGiro,'to_left',Diferencial).
	
%casos de test posición
%posicion(80,70,0,50,40, vl :-59 vr :-38
%posicion(80,70,90,50,40, vl :-38 vr :-59
%posicion(80,70,45,50,40, vl :-49 vr :-48<> -49
%posicion(80,70,135,50,40, vl :-15 vr :15
%posicion(80,70,180,50,40, vl :38 vr :59
%posicion(80,70,225,50,40, vl :48<>49 vr :49
%Test ok



%Ver Casos especiales que evitan divición por 0
atan2(Dy,0,90):-Dy>0.
atan2(Dy,0,-90):-Dy=<0.

atan2(Dy,Dx,X):- %segundo cuadrante
	Dx < 0, Dy >0, X is truncate(180/3.14 * atan(Dy/Dx)+180).
atan2(Dy,Dx,X):- %tercer cuadrante
	Dx < 0, Dy < 0, X is truncate(180/3.14 * atan(Dy/Dx)-180).	
atan2(Dy,Dx,X):- %primero y cuarto cuadrante se dejan como estan
	 Dx>0, X is truncate(180/3.14 * atan(Dy/Dx)).

%ajusta el ángulo T en T2 para que este dentro del rango [-180,180]

ajuste_angulo(T,T2):-
	T =< 180,
	ajuste_angulo2(T,T2).
ajuste_angulo(T,T2):-
	T >180,
	T3 is T - 360,
	ajuste_angulo(T3,T2).
ajuste_angulo2(T,T):- (T > -180).
ajuste_angulo2(T,T2):-
	T =< -180,
	T3 is T + 360,
	ajuste_angulo2(T3,T2).

%devuelve el ángulo que debería moverse hacia el objetivo sumandole el ángulo
%que tiene el robot Rr
angulo_deseado(0,0,Rr,Theta_e):-
	Theta_e is 90 - Rr.
angulo_deseado(Dy,Dx,Rr,Theta_e):-
		atan2(Dy,Dx,Desired_angle),
		Theta_e is Desired_angle - Rr .

%devuelve en Ka la potencia para las ruedas según la distancia al objetivo D_e		
ajuste_Ka(D_e,Ka):-
	D_e > 100,Ka is 17/90.
ajuste_Ka(D_e,Ka):-
	D_e > 50,Ka is 19/90.
ajuste_Ka(D_e,Ka):-
	D_e>30,Ka is 21/90.
ajuste_Ka(D_e,Ka):-
	D_e>20, Ka is 23/90.
ajuste_Ka(_D_e,Ka):-
	Ka is 25/90.

%devuelve la potencia en las dos ruedas dependiendo del ángulo en donde este el objetivo
ajuste_direccion(Theta_e,D_e, Ka,Vc,Vi,Vd):-	
	(Theta_e > 105 ;	Theta_e < -105),%display(95),nl,
		Theta_e2 is Theta_e + 180,
		(Theta_e2 >180, Theta_e3 is Theta_e2 - 360; Theta_e3 is Theta_e2),
		(Theta_e3 >80 ,Theta_e4 is 80; Theta_e4 is Theta_e3),
		(Theta_e4 < -80 ,Theta_e5 is -80; Theta_e5 is Theta_e4),
		(D_e<5 , abs(Theta_e5)<40, Ka2 is 0.1; Ka2 is Ka),
		Vd is truncate(-Vc * (1.0/(1.0 + exp(-3.0 * D_e)) - 0.3) + Ka2 * Theta_e5),
		Vi is truncate(-Vc * (1.0/(1.0 + exp(-3.0 * D_e)) - 0.3) - Ka2 * Theta_e5).
ajuste_direccion(Theta_e,D_e, Ka,Vc,Vi,Vd):-	
	Theta_e < 75,
	Theta_e > -75,
	(D_e<5 , abs(Theta_e)<40, Ka is 0.1;true),
	Vd is truncate(Vc * (1.0/(1.0 + exp(-3.0 * D_e)) - 0.3) + Ka * Theta_e),
	Vi is truncate(Vc * (1.0/(1.0 + exp(-3.0 * D_e)) - 0.3) - Ka * Theta_e).		
		
ajuste_direccion(Theta_e,_D_e, _Ka,_Vc,Vi,Vd):-		
		Vd is truncate(0.17*Theta_e),
		Vi is truncate(-0.17*Theta_e).
 
patear(_Robot, Direccion, Potencia, _Angulo, Vi,Vd) :-
	gira(Vi,Vd,Potencia,Direccion,200).

tiene_pelota(robot('propio',_Number,pos(Xr,Yr,_Z,_Rr))) :-
	pelota(X,Y,_),
	Dx is X-Xr,
	Dy is Y-Yr,
	Distancia is sqrt(Dx**2 + Dy**2),
	nl,
	display('DISTANCIA : '),
	display(Distancia), nl,
	distancia_cero(Distancia).
	
llevar_pelota_to_arco(Robot,Vi,Vd) :-
	ir_a_posicion(Robot,20,41,Vi,Vd).
	
	
