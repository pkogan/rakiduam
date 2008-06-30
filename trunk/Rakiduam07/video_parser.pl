:- module(video_parser,[analyze/4],[dcg]).

:- use_module(library(lists)).
:- use_module(library(file_utils)).
:- use_module(doraemon_tokenizer).
:- use_module(configuration).


% main([File],Pelota,Jugadores):-
% 	file_to_string(File,S),
% 	analize(S,L),
% 	member(robot(pelota,0,pos(XP,YP,ZP,_)),L),
% 	delete_non_ground(L,robot(pelota,0,pos(XP,YP,ZP,_)),Jugadores),
% 	Pelota = pos(XP,YP,ZP).
% %	close(Stream).

% Pelota = pos(X,Y,Z).
% Jugadores = [robot(propio | contrario,NumeroJugador,pos(X,Y,Z,R)),...]



analyze(S,Pelota,Jugadores,Estado):-
	get_element(environment,real),
	analize(S,L),
	Estado = [0,0],
	member(robot(pelota,0,pos(XP,YP,ZP,_)),L),
	delete_non_ground(L,robot(pelota,0,pos(XP,YP,ZP,_)),Jugadores),
	Pelota = pos(XP,YP,ZP).

analyze(S,Pelota,Jugadores,Estado):-
	get_element(environment,simulado),
	posxyz(S,Pelota,MensajeR),
	jugadores(MensajeR,Jugadores,MensajeP),
	estado(MensajeP,Estado,_MensajeN),!.



analize(S,L):-
	salida(L,S,E),
%	estructurar(E,
	display(E).
	

salida(Estado) --> init(Estado).

init(Estado) --> 
						%nextLine(pos(1,1),I1),
	           firstline(N,pos(1,1),I1),
		   nextLine(I1,I2),
		   parseLines(N,I2,_O,Estado).
%		   parseLine(I2,_O,Estado).

firstline(N,I,O) --> nextToken(I,I1,Number), nextToken(I1,I2,_) , nextToken(I2,O,_) , {number_codes(N,Number) }.

parseLines(1,I,O,[Estado]) --> parseLine(I,O,Estado).
parseLines(N,I,O,[Estado|Rest]) --> { N1 is N - 1 }, parseLine(I,I1,Estado) , parseLines(N1,I1,O,Rest).

parseLine(I,O,robot(Equipo,Jug,pos(X,Y,Z,Orientation))) --> 
	                  nextToken(I,I1,_), 
			  nextToken(I1,I2,ObjectNameS),
			         {atom_codes(ObjectName,ObjectNameS),
				 get_player(ObjectName,Jug,Equipo)},
			  nextToken(I2,I3,_Found),
			  nextToken(I3,I4,Xs),{number_codes(X,Xs)},
			  nextToken(I4,I5,Ys),{number_codes(Y,Ys)},
			  nextToken(I5,I6,Zs),{number_codes(Z,Zs)},
			  nextToken(I6,I7,Orientations),{number_codes(Orientation,Orientations)},
			  nextToken(I7,I8,_VelocityX),
			  nextToken(I8,O,_VelocityY).
			  


%funciones de parser in
next_line([],[],[]).
next_line([32|C],[],C).
next_line([10|C],[],C).
next_line([X|C],[X|CL],CR):-
	next_line(C,CL,CR).

%Resuelve el problema de números negativos
numero_string(X,[45|R]):-
	number_codes(X1,R),
	X is -X1.
numero_string(X,R):-
	number_codes(X,R).
next_number([],0,[]).
next_number(Mensaje,X,MensajeS):-
	next_line(Mensaje,StrX,MensajeS),
	%display(StrX),write_string(StrX),nl,%hasta StrXesta bien
	numero_string(X,StrX), %error en la función number_codes para números negativos
	%display(X),
	!.
posxyzr(Stream,pos(X,Y,Z,R),StreamQ):-
	next_number(Stream, X ,StreamY),
        next_number(StreamY, Y,StreamZ),
        next_number(StreamZ, Z,StreamR),
        next_number(StreamR, R,StreamQ).
equipos(Stream,'contrario',[robot('contrario',5,X)],5,StreamS):-
	posxyzr(Stream,X,StreamS).
equipos(Stream,'propio',[robot('propio',5,X)|L],5,StreamS):-
	posxyzr(Stream,X,StreamS1),
	equipos(StreamS1,'contrario',L,1,StreamS). 
equipos(Stream,Equipo,[robot(Equipo,N,X)|L],N,StreamS):-
	posxyzr(Stream,X,StreamS1),
	N2 is N+1,
	equipos(StreamS1,Equipo,L,N2,StreamS).
 jugadores(Stream,ListaJugadores,StreamS):-
	equipos(Stream,'propio',ListaJugadores,1,StreamS).

posxyz(Stream,pos(X,Y,Z),StreamR):-
	next_number(Stream, X,StreamY),
        next_number(StreamY, Y,StreamZ),
        next_number(StreamZ, Z,StreamR).
write_pos(pos(X,Y,Z)):-
	display('('),display(X),display(','),display(Y),display(','),display(Z),display(')').
write_jugadores([]):-nl.
write_jugadores([robot(E,N,pos(X,Y,Z,R))|C]):-
	display('Robot '),display(E),display(' '),display(N), display('('),display(X),display(','),display(Y),display(','),display(Z),display(','),display(R),display(')'),
	write_jugadores(C).
estado(Stream,estado(Estado,TienePelota),StreamQ):-
	next_number(Stream, Estado ,StreamY),
        next_number(StreamY, TienePelota,StreamQ).
write_estado(estado(1,Y)):-
	display('FREE_BALL '),
	write_tienepelota(Y).
write_estado(estado(2,Y)):-
	display('PLACE_KICK '),
	write_tienepelota(Y).
write_estado(estado(3,Y)):-
	display('PENALTY_KICK '),
	write_tienepelota(Y).
write_estado(estado(4,Y)):-
	display('FREE_KICK '),
	write_tienepelota(Y).
write_estado(estado(5,Y)):-
	display('GOAL_KICK '),
	write_tienepelota(Y).
write_estado(estado(_,Y)):-
	display('INDEFINIDO '),
	write_tienepelota(Y).
write_tienepelota(0):-
	display('ANYONES_BALL').
write_tienepelota(1):-
	display('BLUE_BALL').
write_tienepelota(2):-
	display('YELLOW_BALL').
write_tienepelota(_):-
	display('INDEFINIDO').

%// gameState
%const long FREE_BALL = 1;
%const long PLACE_KICK = 2;
%const long PENALTY_KICK = 3;
%const long FREE_KICK = 4;
%const long GOAL_KICK = 5;
%
%// whosBall
%const long ANYONES_BALL = 0;
%const long BLUE_BALL = 1;
%const long YELLOW_BALL = 2;

