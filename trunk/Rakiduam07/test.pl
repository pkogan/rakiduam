:- module(test,[main/0,iniciar/0,send/2,atras/0,del/0,gira/0]).
:- use_module(library(lists)).
:- use_module(library(sockets)).

:- data socket/1.

iniciar:-
       connect_to_socket_type('10.0.10.99',8765,dgram,S),
       set_fact(socket(S)).

send(Vi,Vd):-
	socket(S),
	number_codes(Vi,Vis),
	number_codes(Vd,Vds),
	list_concat(["blue1,",Vis,",",Vds,"\n"],L),
	socket_send(S,L).

detener:-
	socket(S),
	socket_send(S,"blue1,0,0\n"),
	socket_send(S,"blue2,0,0\n"),
	socket_send(S,"blue3,0,0\n").

main:-
	iniciar,
	repeat,	
	detener,
	fail.
%iniciar(S),send(S,30,-30).iniciar(S),send(S,30,-30).

atras:- send(-60,-60).
del:- send(60,60).

gira:- send(-60,60).