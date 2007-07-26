:- module(logger,[iniciarLog/2,escribirLog/3], []).
%main:-
%	iniciarLog('test',Stream),
%	estado(Estado),
%	display(Stream,'hola').
%  	escribirLog(Stream,Estado).


iniciarLog(NombreArchivo,Stream):-
	open(NombreArchivo,write,Stream).
%si no existe lo crea y si existe lo borra y empieza desde el principio
escribirLog(Stream,[Pelota,Jugadores,_Estado],ListaVelocidades):-
	display(Stream,'#'),
	escribir_jugadores(Stream,Jugadores),
	escribir_pelota(Stream,Pelota),
	display(Stream,'# COMENTARIOS:'),
	escribir_lista(Stream,ListaVelocidades,0),nl(Stream), 
	!.
	
escribir_lista(_,[],_).
escribir_lista(Stream,[Iz1|[De1|Cola]],NumeroJugador):-
	display(Stream,'TXT$[  ('),
	display(Stream,Iz1),
	display(Stream,','),
	display(Stream,De1),
	display(Stream,']'),
	display(Stream,NumeroJugador),
	display(Stream,'['),
	ProximoJugador is NumeroJugador+1,
	escribir_lista(Stream,Cola,ProximoJugador).

escribir_pelota(Stream,pos(X,Y,_)):-
	display(Stream,X),display(Stream,' '),display(Stream,Y),display(Stream,' ').
escribir_jugadores(_Stream,[]).
escribir_jugadores(Stream,[robot(_,_,pos(X,Y,_,R))|Resto]):-
	display(Stream,X),display(Stream,' '),display(Stream,Y),display(Stream,' '),display(Stream,R),display(Stream,' '),
	escribir_jugadores(Stream,Resto).

estado([pos(96.3784,48.6945,0.9949),[robot(propio,1,pos(88.0969,50.4385,1.1225,103.1276)),robot(propio,2,pos(72.3429,25.8993,1.1134,-10.1478)),robot(propio,3,pos(76.3296,54.2119,1.1168,-28.2347)),robot(propio,4,pos(32.9595,14.6477,1.1354,73.6859)),robot(propio,5,pos(33.0176,68.1313,1.1347,87.9535)),robot(contrario,1,pos(9.3756,48.7714,1.1542,89.7354)),robot(contrario,2,pos(24.8075,53.9007,1.1433,89.768)),robot(contrario,3,pos(36.9704,38.7222,1.1227,89.8012)),robot(contrario,4,pos(91.7501,34.6633,1.1483,0.4305)),robot(contrario,5,pos(90.5074,31.5034,1.1455,90.4819))],estado(0,0)]).