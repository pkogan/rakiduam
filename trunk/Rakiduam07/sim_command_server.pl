:-module(sim_command_server,[iniciarCS/3,sendCS/2],[assertions]).
:- use_module(library(sockets)).
:- comment(title, "Modulo Interfaz con el servidor de comandos").

:- comment(author, "Pablo Kogan").

:- comment(module, "Este módulo define las primitivas para inicializar el servidor de comandos y enviar los paquetes a este.").

:- comment(doinclude,iniciarCS/3).
:- comment(doinclude,sendCS/2).

:- pred iniciarCS(+HostCS, +PortCS, -StreamCS) ::
        atom * int *  stream
 # "Retorna en @var{StreamCS} un stream conectado a un socket udp en @var{HostCS}:@var{PortCS}.".

%iniciarCS(+HostCS,+PortCS,-StreamCS)
%inicializa un socket udp conectandolo contra un HostCS:PortCS

iniciarCS(HostCS,PortCS,StreamCS):-
        connect_to_socket_type(HostCS, PortCS,dgram, StreamCS), 
        display('Conectado a Command Server en Puerto '),
        display(PortCS),
        display(', Host '),
        display(HostCS),
        display('.'),
        nl.

:- pred sendCS(+StreamCS, +ListaVelocidades) ::
        stream * string
 # "Envia por el socket udp conectado a @var{StreamCS} la lista @var{ListaVelocidades} que representa los comandos para todos los robots.".
%recibirCS(?StreamVS,+ListadeVelocidades)
%Espera recibir los datos del ambiente y los transforma a estructuras de prolog

sendCS(StreamCS,ListaVelocidades):-
	listavel(ListaVelocidades,Lista_Str),
 	socket_send(StreamCS,Lista_Str),!.

%***************************funciones de parser out
listavel([],[10]).
listavel([Numero|Resto],Lista_Str):-
	listavel(Resto,Resto_Str),
	number_codes(Numero,Numero_Str),
	append2(Numero_Str,Resto_Str,Lista_Str).
append2([],L2,[10|L2]).
append2([X|C],L2,[X|L3]):-
	append2(C,L2,L3).

	
	