%:- module(primitivas, [patear_pelota_a_posicion/5,llevar_pelota_a_posicion/5,acompannar_a_lleva_pelota/5,acompannar_a_lleva_pelota2/5,acompannar_a_lleva_pelota_pres/5,acompannar_a_lleva_pelota_def/5,esperar_pelota_en_posicion/7,desbloquear/3,ir_a_posicion_segura/5,ir_a_posicion_segura_pres/5,ir_a_posicion_insegura/5,ir_a_posicion_precisa/5,ir_a_posicion/8,gira/5,goto_position/5,avanzar/3,distancia_entre_puntos/4], [assertions]).
:- module(primitivas,[ir_a_posicion/5,gira/5,resolver/4], [assertions]).
:- use_module(ambiente).


:- comment(title, "Modulo primitivas").

:- comment(author, "Pablo Kogan").

:- comment(module, "Este m�dulo representa las primitivas de movimiento de los agentes.").


:- comment(doinclude,ir_a_posicion/5).



ir_a_posicion(robot('propio',_Number,pos(Xr,Yr,_Z,Rr)), X,Y,Vi,Vd) :-
	Dx is X-Xr,
	Dy is Y-Yr,
	D_e is sqrt(Dx**2 + Dy**2), % pit�agoras distancia al objetivo (X,Y)
	display('dx, dy, D_e'),nl,	display(Dx),nl,	display(Dy),nl,	display(D_e),nl,
	angulo_deseado(Dy,Dx,Rr,Theta_e),
	display(' Theta_e '),	display(Theta_e),	nl,
	ajuste_angulo(Theta_e,Theta_e2),
	display('Angulo ajustado '),	display(Theta_e2),	nl,
	resolver(Theta_e2,D_e,Vi,Vd).


	


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
 # "Este predicado devuelve la accion (@var{Vi}, @var{Vd}, potencia en las ruedas) necesaria para que el robot ubicado en el punto (@var{Xr},@var{Yr}) con una rotacion @var{Rr} y con una presici�n de @var{Vc}, se dirija al punto (@var{X},@var{Y})".

%Devuelve la velocidad en cada rueda Vi Vd para que el robot ubicado en Xr,Yr y rotaci�n Rr
%se dirija a la posici�n X,Y destino

%voy para adelante
error_angular(_D_e,4).

resolver(Theta_e2,D_e,125,125):-
	error_angular(De,Error),
	Theta_e2>(-Error), Theta_e2<Error.
%voy para atras
resolver(Theta_e2,D_e,-125,-125):-
	error_angular(De,Error),
	(Theta_e2>(180-Error) ; Theta_e2<(-180+Error)).
%voy para la izquierda
resolver(Theta_e2,D_e,Vi,Vd):-
	error_angular(D_e,Error),
	Theta_e2>=Error , Theta_e2 < 90,
	PotenciaDeGiro is 100,
	Diferencial is Theta_e2,
	gira(Vi,Vd,PotenciaDeGiro,'to_left',Diferencial).
%voy para la derecha
resolver(Theta_e2,D_e,Vi,Vd):-
	error_angular(D_e,Error),
	Theta_e2<(-Error) , Theta_e2 > (-90),
	PotenciaDeGiro is 100,
	Diferencial is Theta_e2*(-1),
	gira(Vi,Vd,PotenciaDeGiro,'to_right',Diferencial).
	
	
resolver(Theta_e2,D_e,-60,-30):-
	Theta_e2=<(-90).
%voy para la derecha
resolver(Theta_e2,D_e,-30,-60):-
	Theta_e2>=90.
	
%casos de test posici�n
%posicion(80,70,0,50,40, vl :-59 vr :-38
%posicion(80,70,90,50,40, vl :-38 vr :-59
%posicion(80,70,45,50,40, vl :-49 vr :-48<> -49
%posicion(80,70,135,50,40, vl :-15 vr :15
%posicion(80,70,180,50,40, vl :38 vr :59
%posicion(80,70,225,50,40, vl :48<>49 vr :49
%Test ok



%Ver Casos especiales que evitan divici�n por 0
%atan2(Dy,0,90):-Dy>0.
%atan2(Dy,0,-90).

atan2(Dy,Dx,X):- %segundo cuadrante
	Dx < 0, Dy >0, X is truncate(180/3.14 * atan(Dy/Dx)+180).
atan2(Dy,Dx,X):- %tercer cuadrante
	Dx < 0, Dy < 0, X is truncate(180/3.14 * atan(Dy/Dx)-180).	
atan2(Dy,Dx,X):- %primero y cuarto cuadrante se dejan como estan
	 X is truncate(180/3.14 * atan(Dy/Dx)).

%ajusta el �ngulo T en T2 para que este dentro del rango [-180,180]

ajuste_angulo(T,T2):-
	T =< 180,
	ajuste_angulo2(T,T2).
ajuste_angulo(T,T2):-
	T3 is T - 360,
	ajuste_angulo(T3,T2).
ajuste_angulo2(T,T):- (T > -180).
ajuste_angulo2(T,T2):-
	T3 is T + 360,
	ajuste_angulo2(T3,T2).

%devuelve el �ngulo que deber�a moverse hacia el objetivo sumandole el �ngulo
%que tiene el robot Rr
angulo_deseado(0,0,Rr,Theta_e):-
	Theta_e is 90 - Rr.
angulo_deseado(Dy,Dx,Rr,Theta_e):-
		atan2(Dy,Dx,Desired_angle),
		Theta_e is Desired_angle - Rr .

%devuelve en Ka la potencia para las ruedas seg�n la distancia al objetivo D_e		
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

%devuelve la potencia en las dos ruedas dependiendo del �ngulo en donde este el objetivo
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
 
