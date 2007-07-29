:- module(primitivas, [patear_pelota_a_posicion/5,llevar_pelota_a_posicion/5,acompannar_a_lleva_pelota/5,acompannar_a_lleva_pelota2/5,acompannar_a_lleva_pelota_pres/5,acompannar_a_lleva_pelota_def/5,esperar_pelota_en_posicion/7,desbloquear/3,ir_a_posicion_segura/5,ir_a_posicion_segura_pres/5,ir_a_posicion_insegura/5,ir_a_posicion_precisa/5,ir_a_posicion/8], [assertions]).
:- use_module(ambiente).


:- comment(title, "Modulo primitivas").

:- comment(author, "Pablo Kogan").

:- comment(module, "Este m�dulo representa las primitivas de movimiento de los agentes.").


:- comment(doinclude,patear_pelota_a_posicion/5).
:- comment(doinclude,llevar_pelota_a_posicion/5).
:- comment(doinclude,acompannar_a_lleva_pelota/5). 
:- comment(doinclude,acompannar_a_lleva_pelota2/5).
:- comment(doinclude,acompannar_a_lleva_pelota_def/5). 
:- comment(doinclude,esperar_pelota_en_posicion/7).
:- comment(doinclude,desbloquear/3).
:- comment(doinclude,ir_a_posicion_segura/5).
:- comment(doinclude,ir_a_posicion_insegura/5).
:- comment(doinclude,ir_a_posicion_precisa/5).
:- comment(doinclude,ir_a_posicion/7).

%Ver... Problemas
:- pred patear_pelota_a_posicion(+Robot,+Xd,+Yd,-Vi,-Vd) :: atom *int * int * int * int 
 # "Este predicado no esta siendo utilizado.".

patear_pelota_a_posicion(robot(_E,_J,pos(Xr,Yr,_,Rr)),Xd,Yd,Vi,Vd):-
%	display('entro'), 
	DVer is 3.7, %Ver DVer
	DDest is 2.8, % Distancia a la pelota, desde el punto que se dirige el agente para llevar la pelota hacia el destino
	AVer is 30, %Ver �ngulo Diferencia angular 
	pelota_pred(Xball,Yball,_),
	%Xball is 4,Yball is 4,
	Dy_Ball_Dest is Yd-Yball,
	Dx_Ball_Dest is Xd-Xball,
	Dy_Robot_Ball is Yball-Yr,
	Dx_Robot_Ball is Xball-Xr,
	atan2(Dy_Ball_Dest,Dx_Ball_Dest,Ang_Ball_Dest),
%	display('Angulo Pelota Destino '),display(Ang_Ball_Dest),
	atan2(Dy_Robot_Ball,Dx_Robot_Ball,Ang_Robot_Ball),
%	display('Angulo Robot Pelota '),display(Ang_Robot_Ball),
%	display('paso antan'),
	%Calculo la funci�n Pelota_Destino yd = Md.X + Cd
	Md is Dy_Ball_Dest/Dx_Ball_Dest,
	Cd is Yball-Md*Xball,
	Dist_Robot_Ball=sqrt(Dx_Robot_Ball**2+Dy_Robot_Ball**2),
	(Xball>Xd,Signo is 1;Xball<Xd,Signo is -1;Signo is 0),
	Ang_Dif is abs(Ang_Ball_Dest-Ang_Robot_Ball),
%	display(Ang_Dif),nl,

	( %Si el angulo de la pelota al destino y del robot a la pelota difieren en menso de Ave
	  %y si la distancia del robot a la pelota es menor a Dver sigo con direcci�n a la pelota
	  Ang_Dif<AVer, %Ver �ngulo
	  Dist_Robot_Ball<DVer, %Ver DVer
	  %Xnuevo is Xball, Ynuevo is Yball,
%	  display('Va traz la pelota'),
	  %Xnuevo is Xball - Signo* sqrt(DDest**2/(1+Md**2)),
	  %Ynuevo is Md*Xdv +Cd;	    
	  %Xnuevo is Xd,
	  %Ynuevo is Yd;
	  (Ang_Ball_Dest>0,Vi is -125,Vd is 125;Vi is 125,Vd is -125);
	  %Calculo los puntos destino nuevos.

	  Xdv is Xball + Signo* sqrt(DDest**2/(1+Md**2)),
	  Ydv is Md*Xdv +Cd,
	  evacion_obstaculo(Xr,Yr,Xball,Yball,Xdv,Ydv,Xnuevo,Ynuevo,2.5),
	ir_a_posicion_insegura(robot(_E,_J,pos(Xr,Yr,_Zr,Rr)),Xnuevo,Ynuevo,Vi,Vd)
	).
	%display(Xnuevo),nl,display(Ynuevo),nl,

%	ir_a_posicion_segura(Xr,Yr,Rr,Xnuevo,Ynuevo,Vi,Vd).
%********************************************************************************



%Casos de test levar pelota  a posici�n

%main:-
%	Xr is 5,Yr is 3,Xd is 0,Yd is 8,
%	llevar_pelota_a_posicion(robot(_E,_J,pos(Xr,Yr,_,_Rr)),Xd,Yd,Vi,Vd).
%	llevar_pelota_a_posicion(robot(_,_,pos,(1,1,_,_)),1,10,Vi,Vd).
:- pred llevar_pelota_a_posicion(+Robot,+Xd,+Yd,-Vi,-Vd) :: atom * int * int * int * int 
 # "Este predicado devuelve la accion (@var{Vi}, @var{Vd}, potencia en las ruedas) necesaria para que el robot @var{Robot} lleve la pelota al punto (@var{Xd},@var{Yd}).".

llevar_pelota_a_posicion(robot(_E,_J,pos(Xr,Yr,_,Rr)),Xd,Yd,Vi,Vd):-
%	display('entro'), 
	DVer is 3.7, %Ver DVer
	DDest is 2.4, % Distancia a la pelota, desde el punto que se dirige el agente para llevar la pelota hacia el destino
	AVer is 30, %Ver �ngulo Diferencia angular 
	pelota_pred(Xball,Yball,_),
	%Xball is 4,Yball is 4,
	Dy_Ball_Dest is Yd-Yball,
	Dx_Ball_Dest is Xd-Xball,
	Dy_Robot_Ball is Yball-Yr,
	Dx_Robot_Ball is Xball-Xr,
	atan2(Dy_Ball_Dest,Dx_Ball_Dest,Ang_Ball_Dest),
%	display('Angulo Pelota Destino '),display(Ang_Ball_Dest),
	atan2(Dy_Robot_Ball,Dx_Robot_Ball,Ang_Robot_Ball),
%	display('Angulo Robot Pelota '),display(Ang_Robot_Ball),
%	display('paso antan'),
	%Calculo la funci�n Pelota_Destino yd = Md.X + Cd
	Md is Dy_Ball_Dest/Dx_Ball_Dest,
	Cd is Yball-Md*Xball,
	Dist_Robot_Ball=sqrt(Dx_Robot_Ball**2+Dy_Robot_Ball**2),
	(Xball>Xd,Signo is 1;Xball<Xd,Signo is -1;Signo is 0),
	Ang_Dif is abs(Ang_Ball_Dest-Ang_Robot_Ball),
%	display(Ang_Dif),nl,

	( %Si el angulo de la pelota al destino y del robot a la pelota difieren en menso de Ave
	  %y si la distancia del robot a la pelota es menor a Dver sigo con direcci�n a la pelota
	  Ang_Dif<AVer, %Ver �ngulo
	  Dist_Robot_Ball<DVer, %Ver DVer
	  %Xnuevo is Xball, Ynuevo is Yball,
%	  display('Va traz la pelota'),
	  %Xnuevo is Xball - Signo* sqrt(DDest**2/(1+Md**2)),
	  %Ynuevo is Md*Xdv +Cd;	    
	  Xnuevo is Xd,
	  Ynuevo is Yd;

	  %Calculo los puntos destino nuevos.

	  Xdv is Xball + Signo* sqrt(DDest**2/(1+Md**2)),
	  Ydv is Md*Xdv +Cd,
	  evacion_obstaculo(Xr,Yr,Xball,Yball,Xdv,Ydv,Xnuevo,Ynuevo,2.5)
	),
	%display(Xnuevo),nl,display(Ynuevo),nl,
	ir_a_posicion_insegura(robot(_E,_J,pos(Xr,Yr,_Zr,Rr)),Xnuevo,Ynuevo,Vi,Vd).
%	ir_a_posicion_segura(Xr,Yr,Rr,Xnuevo,Ynuevo,Vi,Vd).
%********************************************************************************
:- pred acompannar_a_lleva_pelota(+Robot,+Xd,+Yd,-Vi,-Vd) :: atom * int * int * int * int 
 # "Este predicado devuelve la accion (@var{Vi}, @var{Vd}, potencia en las ruedas) necesaria para que el robot @var{Robot} acompa�e al que lleva la pelota a una distancia razonable (@var{Xd},@var{Yd}).".

acompannar_a_lleva_pelota(robot(_E,_J,pos(Xr,Yr,_,Rr)),Xd,Yd,Vi,Vd):-
%	display('entro'), 
%	DVer is 3.7, %Ver DVer
	ancho_cancha(An),
	DDest is An/6, % Distancia a la pelota, desde el punto que se dirige el agente para llevar la pelota hacia el destino
%	AVer is 30, %Ver �ngulo Diferencia angular 
	pelota_pred(Xball,Yball,_),
	%Xball is 4,Yball is 4,
	Dy_Ball_Dest is Yd-Yball,
	Dx_Ball_Dest is Xd-Xball,
%	Dy_Robot_Ball is Yball-Yr,
%	Dx_Robot_Ball is Xball-Xr,
%	atan2(Dy_Ball_Dest,Dx_Ball_Dest,Ang_Ball_Dest),
%	display('Angulo Pelota Destino '),display(Ang_Ball_Dest),
%	atan2(Dy_Robot_Ball,Dx_Robot_Ball,Ang_Robot_Ball),
%	display('Angulo Robot Pelota '),display(Ang_Robot_Ball),
%	display('paso antan'),
	%Calculo la funci�n Pelota_Destino yd = Md.X + Cd
	Md is Dy_Ball_Dest/Dx_Ball_Dest,
	Cd is Yball-Md*Xball,
%	Dist_Robot_Ball=sqrt(Dx_Robot_Ball**2+Dy_Robot_Ball**2),
	(Xball>Xd,Signo is 1;Xball<Xd,Signo is -1;Signo is 0),
%	Ang_Dif is abs(Ang_Ball_Dest-Ang_Robot_Ball),
%	display(Ang_Dif),nl,

%	( %Si el angulo de la pelota al destino y del robot a la pelota difieren en menso de Ave
	  %y si la distancia del robot a la pelota es menor a Dver sigo con direcci�n a la pelota
%	  Ang_Dif<AVer, %Ver �ngulo
%	  Dist_Robot_Ball<DVer, %Ver DVer
	  %Xnuevo is Xball, Ynuevo is Yball,
%	  display('Va traz la pelota'),
	  %Xnuevo is Xball - Signo* sqrt(DDest**2/(1+Md**2)),
	  %Ynuevo is Md*Xdv +Cd;	    
%	  Xnuevo is Xd,
%	  Ynuevo is Yd;

	  %Calculo los puntos destino nuevos.

	  Xdv is Xball + Signo* sqrt(DDest**2/(1+Md**2)),
	  Ydv is Md*Xdv +Cd,
	  evacion_obstaculo(Xr,Yr,Xball,Yball,Xdv,Ydv,Xnuevo,Ynuevo,7.5), 
%	),
	%display(Xnuevo),nl,display(Ynuevo),nl,
	ir_a_posicion_segura_pres(robot(_E,_J,pos(Xr,Yr,_Zr,Rr)),Xnuevo,Ynuevo,Vi,Vd).
%	ir_a_posicion_segura(Xr,Yr,Rr,Xnuevo,Ynuevo,Vi,Vd).
%********************************************************************************

%********************************************************************************
:- pred acompannar_a_lleva_pelota2(+Robot,+Xd,+Yd,-Vi,-Vd) :: atom * int * int * int * int 
 # "Este predicado devuelve la accion (@var{Vi}, @var{Vd}, potencia en las ruedas) necesaria para que el robot @var{Robot} acompa�e al que lleva la pelota a una distancia razonable (@var{Xd},@var{Yd}).".

acompannar_a_lleva_pelota2(robot(_E,_J,pos(Xr,Yr,_,Rr)),Xd,_Yd,Vi,Vd):-
%	display('entro'), 
%	DVer is 3.7, %Ver DVer
	ancho_cancha(An),
	DDest is An/5, % Distancia a la pelota, desde el punto que se dirige el agente para llevar la pelota hacia el destino
%	AVer is 30, %Ver �ngulo Diferencia angular 
	pelota_pred(Xball,Yball,_),
	(Xball>Xd,Signo is 1;Xball<Xd,Signo is -1;Signo is 0),
	Xdv is Xball + Signo* DDest,
	Ydv is Yball,
	evacion_obstaculo(Xr,Yr,Xball,Yball,Xdv,Ydv,Xnuevo,Ynuevo,10), 
	ir_a_posicion_segura(robot(_E,_J,pos(Xr,Yr,_Zr,Rr)),Xnuevo,Ynuevo,Vi,Vd).


acompannar_a_lleva_pelota_pres(robot(_E,_J,pos(Xr,Yr,_,Rr)),Xd,_Yd,Vi,Vd):-
%	display('entro'), 
%	DVer is 3.7, %Ver DVer
	ancho_cancha(An),
	DDest is An/5, % Distancia a la pelota, desde el punto que se dirige el agente para llevar la pelota hacia el destino
%	AVer is 30, %Ver �ngulo Diferencia angular 
	pelota_pred(Xball,Yball,_),
	(Xball>Xd,Signo is 1;Xball<Xd,Signo is -1;Signo is 0),
	Xdv is Xball + Signo* DDest,
	Ydv is Yball,
	evacion_obstaculo(Xr,Yr,Xball,Yball,Xdv,Ydv,Xnuevo,Ynuevo,10), 
	ir_a_posicion_segura_pres(robot(_E,_J,pos(Xr,Yr,_Zr,Rr)),Xnuevo,Ynuevo,Vi,Vd).

%********************************************************************************


%********************************************************************************
:- pred acompannar_a_lleva_pelota_def(+Robot,+Xd,+Yd,-Vi,-Vd) :: atom * int * int * int * int 
 # "Este predicado devuelve la accion (@var{Vi}, @var{Vd}, potencia en las ruedas) necesaria para que el robot @var{Robot} acompa�e al que lleva la pelota a una distancia razonable entre la pelota y el punto (@var{Xd},@var{Yd}).".

acompannar_a_lleva_pelota_def(robot(_E,_J,pos(Xr,Yr,_,Rr)),Xd,Yd,Vi,Vd):-
%	display('entro'), 
%	DVer is 3.7, %Ver DVer
	ancho_cancha(An),
	DDest is An/6, % Distancia a la pelota, desde el punto que se dirige el agente para llevar la pelota hacia el destino
%	AVer is 30, %Ver �ngulo Diferencia angular 
	pelota_pred(Xball,Yball,_),
	%Xball is 4,Yball is 4,
	Dy_Ball_Dest is Yd-Yball,
	Dx_Ball_Dest is Xd-Xball,
%	Dy_Robot_Ball is Yball-Yr,
%	Dx_Robot_Ball is Xball-Xr,
%	atan2(Dy_Ball_Dest,Dx_Ball_Dest,Ang_Ball_Dest),
%	display('Angulo Pelota Destino '),display(Ang_Ball_Dest),
%	atan2(Dy_Robot_Ball,Dx_Robot_Ball,Ang_Robot_Ball),
%	display('Angulo Robot Pelota '),display(Ang_Robot_Ball),
%	display('paso antan'),
	%Calculo la funci�n Pelota_Destino yd = Md.X + Cd
	Md is Dy_Ball_Dest/Dx_Ball_Dest,
	Cd is Yball-Md*Xball,
%	Dist_Robot_Ball=sqrt(Dx_Robot_Ball**2+Dy_Robot_Ball**2),
	(Xball>Xd,Signo is 1;Xball<Xd,Signo is -1;Signo is 0),
%	Ang_Dif is abs(Ang_Ball_Dest-Ang_Robot_Ball),
%	display(Ang_Dif),nl,

%	( %Si el angulo de la pelota al destino y del robot a la pelota difieren en menso de Ave
	  %y si la distancia del robot a la pelota es menor a Dver sigo con direcci�n a la pelota
%	  Ang_Dif<AVer, %Ver �ngulo
%	  Dist_Robot_Ball<DVer, %Ver DVer
	  %Xnuevo is Xball, Ynuevo is Yball,
%	  display('Va traz la pelota'),
	  %Xnuevo is Xball - Signo* sqrt(DDest**2/(1+Md**2)),
	  %Ynuevo is Md*Xdv +Cd;	    
%	  Xnuevo is Xd,
%	  Ynuevo is Yd;

	  %Calculo los puntos destino nuevos.

	  Xdv is Xball - Signo* sqrt(DDest**2/(1+Md**2)),
	  Ydv is Md*Xdv +Cd,
	  evacion_obstaculo(Xr,Yr,Xball,Yball,Xdv,Ydv,Xnuevo,Ynuevo,10), 
%	),
	%display(Xnuevo),nl,display(Ynuevo),nl,
	ir_a_posicion_segura(robot(_E,_J,pos(Xr,Yr,_Zr,Rr)),Xnuevo,Ynuevo,Vi,Vd).
%	ir_a_posicion_segura(Xr,Yr,Rr,Xnuevo,Ynuevo,Vi,Vd).
%********************************************************************************






:-pred esperar_pelota_en_posicion(+Robot,+X,+Y,+Xd,+Yd,-Vi,-Vd) :: atom * int * int * int * int 
 # "Este predicado devuelve la accion (@var{Vi}, @var{Vd}, potencia en las ruedas) necesaria para que el robot @var{Robot} se ubique en la posicion (@var{X},@var{Y}), con una orientacion tal, para que luego de que la pelota impacte con el, salga en direccion al punto  (@var{Xd},@var{Yd}).".

esperar_pelota_en_posicion(robot(_E,_J,pos(Xr,Yr,_,Rr)),Xball,Yball,Xd,Yd,Vi,Vd):-
%	display('entro'), 
	DVer is 3.7, %Ver DVer
	DDest is 2.4,
	AVer is 30, %Ver �ngulo
	%pelota_pred(Xball,Yball,_),
	%Xball is 4,Yball is 4,
	Dy_Ball_Dest is Yd-Yball,
	Dx_Ball_Dest is Xd-Xball,
	Dy_Robot_Ball is Yball-Yr,
	Dx_Robot_Ball is Xball-Xr,
	atan2(Dy_Ball_Dest,Dx_Ball_Dest,Ang_Ball_Dest),
%	display('Angulo Pelota Destino '),display(Ang_Ball_Dest),
	atan2(Dy_Robot_Ball,Dx_Robot_Ball,Ang_Robot_Ball),
%	display('Angulo Robot Pelota '),display(Ang_Robot_Ball),
%	display('paso antan'),
	%Calculo la funci�n Pelota_Destino yd = Md.X + Cd
	Md is Dy_Ball_Dest/Dx_Ball_Dest,
	Cd is Yball-Md*Xball,
	Dist_Robot_Ball=sqrt(Dx_Robot_Ball**2+Dy_Robot_Ball**2),
	(Xball>Xd,Signo is 1;Xball<Xd,Signo is -1;Signo is 0),
	Ang_Dif is abs(Ang_Ball_Dest-Ang_Robot_Ball),
%	display(Ang_Dif),nl,

	( %Si el angulo de la pelota al destino y del robot a la pelota difieren en menso de Ave
	  %y si la distancia del robot a la pelota es menor a Dver sigo con direcci�n a la pelota
	  Ang_Dif<AVer, %Ver �ngulo
	  Dist_Robot_Ball<DVer, %Ver DVer
	  %Xnuevo is Xball, Ynuevo is Yball,
%	  display('Va traz la pelota'),
	  %Xnuevo is Xball - Signo* sqrt(DDest**2/(1+Md**2)),
	  %Ynuevo is Md*Xdv +Cd;	    
	  Xnuevo is Xd,
	  Ynuevo is Yd;

	  %Calculo los puntos destino nuevos.

	  Xdv is Xball + Signo* sqrt(DDest**2/(1+Md**2)),
	  Ydv is Md*Xdv +Cd,
	  evacion_obstaculo(Xr,Yr,Xball,Yball,Xdv,Ydv,Xnuevo,Ynuevo,3)
	),
	%display(Xnuevo),nl,display(Ynuevo),nl,
	ir_a_posicion_precisa(robot(_E,_J,pos(Xr,Yr,_Zr,Rr)),Xnuevo,Ynuevo,Vi,Vd).
%	ir_a_posicion_segura(Xr,Yr,Rr,Xnuevo,Ynuevo,Vi,Vd).
%********************************************************************************
%no esta siendo usado
evacion_obstaculo_pelota(X1,Y1,Xo,Yo,Xb,Yb,Xb,Yb):-%no es obstaculo
	Xo<(X1-3),Xo<(Xb+3);
	Xo>(X1+3),Xo>(Xb-3);
	Yo<(Y1-3),Yo<(Yb+3);
	Yo>(Y1+3),Yo>(Yb-3).

%idem a evacion obstaculo con la diferencia que el obstaculo es mas chico.
evacion_obstaculo_pelota(X1,Y1,Xo,Yo,Xb,Yb,Xd,Yd):-
	%C�lculo Recta (Robot-Pelota) Yp = Mp*X + Cp 
	Dx is Xb-X1,
	Dy is Yb-Y1,
	%D_b is sqrt(Dx**2 + Dy**2), % pitagoras distancia al objetivo (Xb,Yb)
	Mp is Dy/Dx,
	Cp is Y1-Mp*X1,
	%C�lculo Recta perpendicular a Yp Yobs=Mobs*X + Cobs
	Mobs is -(1/Mp),
	Cobs is Yo-Mobs*Xo,
	%C�lculo del punto intersecci�n de las rectas
	Xc is (Cobs-Cp)/(Mp-Mobs),
	Yc is Mp*Xc+Cp,
	%C�lculo de distancia del obstaculo a la intersecci�n de las rectas
	D is sqrt((Xc-Xo)**2+(Yc-Yo)**2),
	(D<3.5, %Asumo  como la diagonal mayor del robot
		%C�lculo (Xs,Ys) (Xi,Yi) puntos alternativos para evadir el obstaculo
		% a Dt = 2 del obstaculo
		Raiz is sqrt(12.25 /(1+Mobs**2)), %3.5**2=12.25 Raiz es la diferencia en x
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
		%else no es obst�culo
		Xd is Xb, Yd is Yb
	).

%********************************************************************************
%********************************************************************************
:- pred desbloquear(+Robot,-Vi,-Vd) :: atom *  int * int 
 # "Este predicado devuelve la accion (@var{Vi}, @var{Vd}, potencia en las ruedas) necesaria para que el robot @var{Robot} gire en el lugar a maxima velosidad en el sentido contrario al que se estaba intentando mover.".

%evitar Atascamientos
%un jugador es considrerado atascado si:
%la potencia de acci�n anterior es diferente a 0 en las dos ruedas 	Viprev=\=0,Vdprev=\=0,
%la potencia de acci�n promedio acumulada difiere en menos de 12.5 con la potencia de acci�n tamada en el evento anterior
%un factor de desplazamiento calculado en base a los dos �ltimos eventos es menor que un factor calculado en base a la potencia promedio en las dos ruedas del evento anterior.

%Luego la potencia en las dos redas es para girar a m�xima velocidad en el sentido contrario al que se estaba intentando mover.


desbloquear(robot(_E,J,pos(_Xr,_Yr,_,_Rr)),Vi,Vd):-
	%jugador_prev(robot(E,J,pos(Xprev,Yprev,_,Rprev))),
	%display(Xprev),nl,
	accion_prev(J,Viprev,Vdprev),
	%accion_prom(J,Viprom,Vdprom),
	%display(Viprev),nl,display(Viprom),nl,
	Signo is (Vdprev-Viprev)/abs(Vdprev-Viprev),
	((Viprev+Vdprev)=\=0,
	Vi is -Signo*125, %Vdpred*1.5,
	Vd is Signo*125;
	Vi is Signo*125, %Vdpred*1.5,
	Vd is -Signo*125).
	%-Vipred*1.5.
	


%********************************************************************************
:- pred ir_a_posicion_segura(+Robot,+X,+Y,-Vi,-Vd) :: atom * int * int * int * int 
 # "Este predicado devuelve la accion (@var{Vi}, @var{Vd}, potencia en las ruedas) necesaria para que el robot @var{Robot} se dirija al punto (@var{Xd},@var{Yd}), evadiendo los obstaculos que existan en el camino.".

ir_a_posicion_segura(robot(_E,_J,pos(Xr,Yr,_Zr,Rr)),Xsel,Ysel,Iz,De):-
	jugadores(Jugadores),
	ordenar(Jugadores,Jordenado,Xr,Yr),
	%display(Jordenado),
	evitar_obstaculos(Jordenado,Xr,Yr,Xsel,Ysel,Xnuevo,Ynuevo),
	ir_a_posicion(Xr,Yr,Rr,Xnuevo,Ynuevo,170,Iz,De).
ir_a_posicion_segura_pres(robot(_E,_J,pos(Xr,Yr,_Zr,Rr)),Xsel,Ysel,Iz,De):-
	jugadores(Jugadores),
	ordenar(Jugadores,Jordenado,Xr,Yr),
	%display(Jordenado),
	evitar_obstaculos(Jordenado,Xr,Yr,Xsel,Ysel,Xnuevo,Ynuevo),
	ir_a_posicion(Xr,Yr,Rr,Xnuevo,Ynuevo,125,Iz,De).


ordenar([],[],_Xr,_Yr).
ordenar(L,[X|R],Xr,Yr):-
	min(L,X,Xr,Yr),
	del(L,X,L2),
	ordenar(L2,R,Xr,Yr).
min([Robot],Robot,_,_).
min([Robot|R],Menor,Xr,Yr):-
	min(R,M1,Xr,Yr),
	menor(Robot,M1,Xr,Yr,Menor).
menor(robot(_E1,_J1,pos(Xr1,Yr1,_Zr1,_Rr1)),robot(E2,J2,pos(Xr2,Yr2,Zr2,Rr2)),X,Y,robot(E2,J2,pos(Xr2,Yr2,Zr2,Rr2))):-
	sqrt((X-Xr1)**2 + (Y-Yr1)**2)>sqrt((X-Xr2)**2 + (Y-Yr2)**2).
menor(robot(E1,J1,pos(Xr1,Yr1,Zr1,Rr1)),robot(_E2,_J2,pos(Xr2,Yr2,_Zr2,_Rr2)),X,Y,robot(E1,J1,pos(Xr1,Yr1,Zr1,Rr1))):-
	sqrt((X-Xr1)**2 + (Y-Yr1)**2)=<sqrt((X-Xr2)**2 + (Y-Yr2)**2).
%menor(robot(E1,J1,pos(Xr1,Yr1,Zr1,Rr1)),robot(_E2,_J2,pos(_Xr2,_Yr2,_Zr2,_Rr2)),_X,_Y,robot(E1,J1,pos(Xr1,Yr1,Zr1,Rr1))).
del([],_,[]).
del([X|R],X,R).
del([Y|R],X,[Y|L]):-
	del(R,X,L).


evitar_obstaculos([],_Xr,_Yr,X,Y,X,Y).
evitar_obstaculos([robot(_,_N,pos(Xr,Yr,_Zr,_Rr))|R],Xr,Yr,X,Y,Xd,Yd):-
	evitar_obstaculos(R,Xr,Yr,X,Y,Xd,Yd).
evitar_obstaculos([robot(_,_N,pos(Xo,Yo,_Zr,_Rr))|R],X1,Y1,Xb,Yb,Xd,Yd):-
%	evacion_obstaculo(X1,Y1,Xo,Yo,Xb,Yb,Xd1,Yd1),
	evitar_obstaculos(R,X1,Y1,Xb,Yb,Xd2,Yd2),
	%(Xd1==Xd2,Yd1==Yd2,Xd is Xd1,Yd is Yd1;
	evacion_obstaculo(X1,Y1,Xo,Yo,Xd2,Yd2,Xd,Yd,5).
	%evitar_obstaculos([robot(_N,pos(Xo,Yo,_Zr,_Rr))|R],X1,Y1,Xd2,Yd2,Xd,Yd)).


%************************************************************	
%evaci�n del obstaculo Xo,Yo
%robot en posici�n X1,Y1
%destino inicial posici�n Xb,Yb

evacion_obstaculo(X1,Y1,Xo,Yo,Xb,Yb,Xb,Yb,_Dist_Obst):-%no es obstaculo
	Xo<(X1-3),Xo<(Xb);
	Xo>(X1+3),Xo>(Xb);
	Yo<(Y1-3),Yo<(Yb);
	Yo>(Y1+3),Yo>(Yb).
evacion_obstaculo(X1,Y1,Xo,Yo,Xb,Yb,Xd,Yd,Dist_Obst):-
	%C�lculo Recta (Robot-Pelota) Yp = Mp*X + Cp 
	Dx is Xb-X1,
	Dy is Yb-Y1,
	%D_b is sqrt(Dx**2 + Dy**2), % pitagoras distancia al objetivo (Xb,Yb)
	Mp is Dy/Dx,
	Cp is Y1-Mp*X1,
	%C�lculo Recta perpendicular a Yp Yobs=Mobs*X + Cobs
	Mobs is -(1/Mp),
	Cobs is Yo-Mobs*Xo,
	%C�lculo del punto intersecci�n de las rectas
	Xc is (Cobs-Cp)/(Mp-Mobs),
	Yc is Mp*Xc+Cp,
	%C�lculo de distancia del obstaculo a la intersecci�n de las rectas
	D is sqrt((Xc-Xo)**2+(Yc-Yo)**2),
	(D<3.5, %Asumo 3.5 como la diagonal mayor del robot
		%C�lculo (Xs,Ys) (Xi,Yi) puntos alternativos para evadir el obstaculo
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
		%else no es obst�culo
		Xd is Xb, Yd is Yb
	).

%************************************************************	
%********************************************************************************
:- pred ir_a_posicion_insegura(+Robot,+X,+Y,-Vi,-Vd) :: atom * int * int * int * int 
 # "Este predicado devuelve la accion (@var{Vi}, @var{Vd}, potencia en las ruedas) necesaria para que el robot @var{Robot} se dirija al punto (@var{Xd},@var{Yd}), sin evadir los obstaculos que existan en el camino.".

ir_a_posicion_insegura(robot(_E,_J,pos(Xr,Yr,_Zr,Rr)),X,Y,Iz,De):-
	ir_a_posicion(Xr,Yr,Rr,X,Y,170,Iz,De).



%********************************************capa mas baja

%********************************************************************************
:- pred ir_a_posicion_precisa(+Robot,+X,+Y,-Vi,-Vd) :: atom * int * int * int * int 
 # "Este predicado devuelve la accion (@var{Vi}, @var{Vd}, potencia en las ruedas) necesaria para que el robot @var{Robot} se dirija al punto (@var{Xd},@var{Yd}), sin evadir los obstaculos que existan en el camino.".

ir_a_posicion_precisa(robot(_E,_J,pos(Xr,Yr,_Zr,Rr)),X,Y,Iz,De):-
	ir_a_posicion(Xr,Yr,Rr,X,Y,125,Iz,De).



%********************************************capa mas baja


:- pred ir_a_posicion(+Xr,+Yr,+Rr,+X,+Y,+Vc,-Vi,-Vd) :: int * int * int * int * int * int * int * int 
 # "Este predicado devuelve la accion (@var{Vi}, @var{Vd}, potencia en las ruedas) necesaria para que el robot ubicado en el punto (@var{Xr},@var{Yr}) con una rotacion @var{Rr} y con una presici�n de @var{Vc}, se dirija al punto (@var{X},@var{Y})".

%Devuelve la velocidad en cada rueda Vi Vd para que el robot ubicado en Xr,Yr y rotaci�n Rr
%se dirija a la posici�n X,Y destino

ir_a_posicion(Xr,Yr,Rr,X,Y,Vc,Vi,Vd):-
%	Vi is 70,
%	Vd is 70,
	%Vc is 100,
	%Vc is 150, %Creo que esta bien 150 
	%Ka is 0.0/90.0,
	Dx is X-Xr,
	Dy is Y-Yr,
	D_e is sqrt(Dx**2 + Dy**2), % pit�agoras distancia al objetivo (X,Y)
%	display('dx, dy, D_e'),nl,	display(Dx),nl,	display(Dy),nl,	display(D_e),nl,
	angulo_deseado(Dy,Dx,Rr,Theta_e),
%	display(' Theta_e '),	display(Theta_e),	nl,
	ajuste_angulo(Theta_e,Theta_e2),
%	display('Angulo ajustado '),	display(Theta_e2),	nl,
	ajuste_Ka(D_e,Ka),
%	display('Ka :'),	display(Ka),nl,
	ajuste_direccion(Theta_e2,D_e, Ka,Vc,Vi,Vd).

%casos de test posici�n
%posicion(80,70,0,50,40, vl :-59 vr :-38
%posicion(80,70,90,50,40, vl :-38 vr :-59
%posicion(80,70,45,50,40, vl :-49 vr :-48<> -49
%posicion(80,70,135,50,40, vl :-15 vr :15
%posicion(80,70,180,50,40, vl :38 vr :59
%posicion(80,70,225,50,40, vl :48<>49 vr :49
%Test ok


%main:- 	
%	X is 80, Y is 20, A is -180, XP is 0, YP is 0,
%	Xo is 42, Yo is 12,
%	evacion_obstaculo(X,Y,Xo,Yo,XP,YP,XN,YN),
%	display('X nuevo '),display(XN),nl,
%	display('Y nuevo '),display(YN).
%	para test posici�n
%	X is 80, Y is 70, A is -180, XP is 50, YP is 40,
%	DA is 10, AF is 180,
%	display('robot '),display(X),display(','),display(Y),nl,
%	display('pelota '),display(XP),display(','),display(YP),nl,
%	vuelta(X,Y,XP,YP,A,DA,AF).
% funcion para test navegaci�n
vuelta(_X,_Y,_XP,_YP,A,_DA,AF):-
	A>AF.
vuelta(X,Y,XP,YP,A,DA,AF):-	
	ir_a_posicion(X,Y,A,XP,YP,150,Vi,Vd),
	display('angulo '),display(A),display(' '),
	display('vl :'),
	display(Vi),
	display('vr :'),
	display(Vd),nl,
	AA is A+DA,
	vuelta(X,Y,XP,YP,AA,DA,AF).

%funci�n que devuelve el �ngulo X = arctan(Dy/Dx) seg�n el cuadrante en en rango [-180, 180]

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


