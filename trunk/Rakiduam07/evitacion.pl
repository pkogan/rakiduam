%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   2008 Patricio Biondelli, Germán Braun, Sergio Cotal, Daniel Trevisani
%
%   Módulo de Evasión de obstáculos basado en Métodos de Campos
%   de Potencial.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(evitacion,[evitar/3,discretizarJug/3,discretizar/2,distancia_obstaculo_mas_cercano/5],[assertions]).
:- use_module(ambiente).
:- use_module(library(lists)).

:- data[pos_minima/1].


get_environment('simulado').

real_simulado(S):- get_environment(S).

minX(7):- real_simulado('simulado').
minX(0):- real_simulado('real').

minY(0):- real_simulado('real').
minY(6):- real_simulado('simulado').

tamCelda(7):- real_simulado('simulado').
tamCelda(200):- real_simulado('real').


% Cancha: 12 X 10 celdas.
% cancha(6.8118,6.3730,93.4259,77.2392).

% Aserciones:
% pos_minima(pos(X,Y)).


% Define la interfaz con el módulo de evitación.
% Devuelve una posición libre de obstáculos en función
% de una meta dada.
evitar(pos(NumRob,Xr,Yr),PosMeta,PosEvitada):-  
          jugadores(Jugadores),
          filtrarJug_propio(Jugadores,NumRob,JugadoresC),
          discretizarJug(Jugadores,NumRob,JugadoresD),
          distancia_obstaculo_mas_cercano(pos(Xr,Yr),JugadoresC,Dd,Dc,PosMasCercanoC),
          obtener_posicion(Dd,Dc,pos(Xr,Yr),JugadoresD,PosMeta,PosMasCercanoC,PosEvitada).

% Determina la distancia a la cual se considera
% que el robot propio está muy cerca ("pegado") de un obstáculo.
pegado(6):- real_simulado('simulado').
pegado(200):- real_simulado('real').

% Se obtiene la posición a la cual se dirigirá el robot, eventualmente
% eludiendo posibles obstáculos.
obtener_posicion(_,Dc,PosRobotC,_Jugadores,_PosMeta,PosMasCercanoC,PosEvitada):- 
          pegado(Valor), Dc<Valor,
          despegar_jugador(PosRobotC,PosMasCercanoC,PosEvitada). 
obtener_posicion(Dist,_,PosRobotC,Jugadores,PosMeta,_,PosEvitada):- 
          controlar_distancia(Dist,D1),
          discretizar(PosRobotC,PosRobotD),
          definir_ventana(PosRobotD,D1,Lista),
          definir_posicion(Lista,PosRobotC,D1,Jugadores,PosMeta,PosEvitada).


constante_despegue(10):- real_simulado('simulado').
constante_despegue(250):- real_simulado('real').

% Cuando se produce una colisión inevitable, en muchos casos el robot
% queda "pegado" a un obstáculo. En este caso, se procede a separar ("despegar")
% al robot del obstáculo haciendo que el mismo se mueva en sentido inverso al de
% la posicion del obstáculo.
despegar_jugador(pos(Xr,Yr),pos(Xcer,Ycer),pos(X,Yr)):- 
          Xr>Xcer, prevaleceX(Xr,Yr,Xcer,Ycer), 
          constante_despegue(C), X is Xr+C,!.
despegar_jugador(pos(Xr,Yr),pos(Xcer,Ycer),pos(X,Yr)):- 
          Xr<Xcer, prevaleceX(Xr,Yr,Xcer,Ycer), 
          constante_despegue(C), X is Xr-C,!.
despegar_jugador(pos(Xr,Yr),pos(_Xcer,Ycer),pos(Xr2,Y)):-  constante_despegue(C),
          Yr>Ycer,Xr2 is Xr-C, 
         Y is Yr+C,!.
despegar_jugador(pos(Xr,Yr),pos(_Xcer,Ycer),pos(Xr2,Y)):-  constante_despegue(C),
          Yr<Ycer,Xr2 is Xr-C, 
          Y is Yr-C,!.
%no se da nunca, pero por las dudas........
%despegar_jugador(pos(Xr,Yr),_,pos(Xr,Yr)):- display('misma pos  '),nl,!.


% Determina si el robot esta mas alejado del obstaculo en X que en Y.
prevaleceX(X1,Y1,X2,Y2):- abs(X1-X2) >= abs(Y1-Y2).  


maximo_frontera(7):- real_simulado('simulado').
maximo_frontera(200):- real_simulado('real').

% Se determina un radio maximo de frontera.
controlar_distancia(D,D):- maximo_frontera(Max), D =< Max.
controlar_distancia(_,Max):- maximo_frontera(Max).

% Discretiza las posiciones de los jugadores y a su vez
% excluye al jugador de turno.
discretizarJug([],_,[]).
discretizarJug([(robot(Clase,Numero,pos(X,Y,_,_)))|T],NumRob,[pos(Xd,Yd)|Td]):- 
          \+ mismo_jugador(Clase,Numero,NumRob),
          discretizar(pos(X,Y),pos(Xd,Yd)),
          discretizarJug(T,NumRob,Td).
discretizarJug([_|T],NumRob,Td):-
        discretizarJug(T,NumRob,Td). 


% Genera una nueva lista de los jugadores pero excluyendo
% al jugador de turno. (Valores originales de las posiciones).
filtrarJug_propio([],_,[]).
filtrarJug_propio([(robot(Clase,Numero,pos(X,Y,_,_)))|T],NumRob,[pos(X,Y)|Td]):- 
          \+ mismo_jugador(Clase,Numero,NumRob),
             filtrarJug_propio(T,NumRob,Td).
filtrarJug_propio([_|T],NumRob,Td):-
        filtrarJug_propio(T,NumRob,Td). 


% Determina si se trata del robot de turno para si poder
% excluirlo de la nueva lista de obstaculos.
mismo_jugador('propio',Numero,Numero).


% Calcula la distancia entre dos puntos de la cancha.
distancia_e(pos(X1,Y1),pos(X2,Y2),D):- D is sqrt((X1-X2)**2 + (Y1-Y2)**2).


% Se discretizan las medidas (Descomposición en celdas) de la cancha en función 
% del tamaño de la celda.
discretizar(pos(X,Y),pos(Xd,Yd)):- minX(Xm),minY(Ym), tamCelda(Tc), Xd is (X-Xm)//Tc, Yd is (Y-Ym)//Tc.

% Se realiza el proceso inverso al anterior para si poder retornar
% las posiciones reales en la cancha. Este proceso contempla un mínimo
% error al intentar obtener las posiciones reales iniciales.
concretizar(pos(X,Y),pos(Xd,Yd)):- minX(Xm),minY(Ym), tamCelda(Tc), Xd is (X*Tc)+Xm+Tc/2, Yd is (Y*Tc)+Ym+Tc/2.


%la pelota esta dentro de la frontera
% definir_posicion(_Lista,PosRobot,Dd,_ListaJugD,PosMeta,PosMeta):- 
% 	distancia_e(PosMeta,PosRobot,D1),Dd>0, D1<(Dd*7)-7,!.
% definir_posicion(_Lista,PosRobot,Dd,_ListaJugD,PosMeta,PosMeta):- 
% 	distancia_e(PosMeta,PosRobot,D1),D1<(Dd*7).

definir_posicion(_Lista,PosRobot,Dd,_ListaJugD,PosMeta,PosMeta):- 
 	distancia_e(PosMeta,PosRobot,D1),Dd>0, tamCelda(T),minX(Mx),
	D1<(Dd*T)-Mx,!.

definir_posicion(_Lista,PosRobot,Dd,_ListaJugD,PosMeta,PosMeta):- 
 	distancia_e(PosMeta,PosRobot,D1),tamCelda(T),D1<(Dd*T).

definir_posicion(Lista,_PosR,_D,ListaJugD,PosMeta,pos(Xc,Yc)):- 
          discretizar(PosMeta,PosMetaD),
          evaluar_celdas(Lista,ListaJugD,PosMetaD,ListaFuerza),
          mejor_celda(ListaFuerza,-500,PosMetaD,pos(50,50),pos(X,Y)),
          concretizar(pos(X,Y),pos(Xc,Yc)).

% Valores de calibracion para las fuerzas
constante_repul(45).
constante_atrac(800).


frep_celda(_,[],Ac,Ac).
frep_celda(pos(Xc,Yc),[pos(Xr,Yr)|T],Ac,F):- 
                    distancia_e(pos(Xc,Yc),pos(Xr,Yr),D),
                    constante_repul(CR), 
	            Ac1 is Ac+(CR/D), 
                    frep_celda(pos(Xc,Yc),T,Ac1,F).

fatrac_celda(pos(Xc,Yc),pos(Xm,Ym),F):- 
                distancia_e(pos(Xc,Yc),pos(Xm,Ym),D),
                constante_atrac(CA),
                F is CA/D.


% Se calculan las fuerzas para cada celda
fuerza_celda(pos(Xc,Yc),ListaJugD,PosMeta,F):- 
	                             frep_celda(pos(Xc,Yc),ListaJugD,0,Fr), 
	                             fatrac_celda(pos(Xc,Yc),PosMeta,Fa), F is Fa-Fr.


evaluar_celdas([],_,_,[]).
evaluar_celdas([pos(Xc,Yc)|T],ListaJugD,PosMeta,[pos(Xc,Yc,F)|T1]):-
               fuerza_celda(pos(Xc,Yc),ListaJugD,PosMeta,F),
               evaluar_celdas(T,ListaJugD,PosMeta,T1).

% Se determina cual es la celda mas conveniente para que el robot
% pueda avanzar.
mejor_celda([],_,_,C,C).
mejor_celda([pos(Xc,Yc,Fc)|T],MinF,PosMetaD,C1,C):- 
                 Fc>MinF, distancia_e(pos(Xc,Yc),PosMetaD,D),
                 distancia_e(C1,PosMetaD,D1), D < D1,
                 mejor_celda(T,Fc,PosMetaD,pos(Xc,Yc),C),!.
mejor_celda([_|T],M,PosMetaD,C1,C):- mejor_celda(T,M,PosMetaD,C1,C).

% Controla que la celda no "caiga" en una zona fuera de los 
% límites de la cancha.
celda_valida(pos(X,Y)):- 
           tamCelda(T),cancha(Xi,Yi,Xs,Ys), 
           ((Xs-Xi)/T) >= X, X >= 0, Y =< ((Ys-Yi)/T), Y >= 0.

lado_inferior(_D,_,Li,Ls,[]):- Ls<Li.
lado_inferior(D,pos(Xr,Yr),Li,Ls,[pos(X,Y)|ListaI]):- 
            Li=<Ls, X is Li, 
            Y is (Yr-D), Li1 is Li+1,
            celda_valida(pos(X,Y)),
            lado_inferior(D,pos(Xr,Yr),Li1,Ls,ListaI).   
lado_inferior(D,pos(Xr,Yr),Li,Ls,ListaI):- 
            Li1 is Li+1,
            lado_inferior(D,pos(Xr,Yr),Li1,Ls,ListaI).   


lado_superior(_D,_,Li,Ls,[]):- Ls<Li.
lado_superior(D,pos(Xr,Yr),Li,Ls,[pos(X,Y)|ListaI]):- 
            Li=<Ls, X is Li, 
            Y is (Yr+D), Li1 is Li+1,
            celda_valida(pos(X,Y)),
            lado_superior(D,pos(Xr,Yr),Li1,Ls,ListaI).   
lado_superior(D,pos(Xr,Yr),Li,Ls,ListaI):- 
            Li1 is Li+1,
            lado_superior(D,pos(Xr,Yr),Li1,Ls,ListaI).   


lado_izquierdo(_D,_,Li,Ls,[]):- Ls<Li.
lado_izquierdo(D,pos(Xr,Yr),Li,Ls,[pos(X,Y)|ListaI]):- 
            Li=<Ls, X is (Xr-D), 
            Y is Li, Li1 is Li+1,
            celda_valida(pos(X,Y)), 
            lado_izquierdo(D,pos(Xr,Yr),Li1,Ls,ListaI).   
lado_izquierdo(D,pos(Xr,Yr),Li,Ls,ListaI):- 
            Li1 is Li+1,
            lado_izquierdo(D,pos(Xr,Yr),Li1,Ls,ListaI).   


lado_derecho(_D,_,Li,Ls,[]):- Ls<Li.
lado_derecho(D,pos(Xr,Yr),Li,Ls,[pos(X,Y)|ListaI]):- 
            Li=<Ls,  X is (Xr+D),
            Y is Li, Li1 is Li+1,
            celda_valida(pos(X,Y)),
            lado_derecho(D,pos(Xr,Yr),Li1,Ls,ListaI).   
lado_derecho(D,pos(Xr,Yr),Li,Ls,ListaI):- 
            Li1 is Li+1,
            lado_derecho(D,pos(Xr,Yr),Li1,Ls,ListaI).   



%controlar_truncate(Dc,1):- Dc < 1,!.
%controlar_truncate(Dc,Dd):- tamCelda(T), Dd is truncate(Dc/T).

% Se determina el radio de la frontera en función de la distancia
% al obstáculo más cercano.
distancia_obstaculo_mas_cercano(PosRobotC,LjugC,Dd,Dc,PosCercanoC):- 
                 minimo(M),  min(LjugC,PosRobotC,M,Dc,PosCercanoC),
                 tamCelda(T), Dd is round(Dc/T).
                 %controlar_truncate(Dc,Dd).
 
% Generación de la frontera de evaluación.
definir_ventana(pos(Xr,Yr),D, L):- Xiz is Xr-D, Xde is Xr+D, Ysup is (Yr+D)-1, Yinf is (Yr-D)+1, 
         lado_superior(D,pos(Xr,Yr),Xiz,Xde,ListaS),
         lado_inferior(D,pos(Xr,Yr),Xiz,Xde,ListaI),
         lado_izquierdo(D,pos(Xr,Yr),Yinf,Ysup,ListaIz),
         lado_derecho(D,pos(Xr,Yr),Yinf,Ysup,ListaDe),
         append(ListaS,ListaI,Laux1),append(ListaIz,ListaDe,Laux2),append(Laux1,Laux2,L). 


minimo(10000).


min([],_,M,M,pos(X,Y)):- pos_minima(pos(X,Y)).
min([pos(Xj,Yj)|T],pos(Xr,Yr),M,R,MasCercano):- 
	distancia_e(pos(Xj,Yj),pos(Xr,Yr),D),
	D<M, 
        retractall_fact(pos_minima(_)),
        asserta_fact(pos_minima(pos(Xj,Yj))),
	min(T,pos(Xr,Yr),D,R,MasCercano),!.
min([_|T],pos(Xr,Yr),M,R,MasCercano):- 
        min(T,pos(Xr,Yr),M,R,MasCercano).
