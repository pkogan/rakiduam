:- module(detener,_,_).

:- use_module(library(sockets)).

iniciar:- 
	connect_to_socket_type(HostCS, PortCS,dgram, StreamCS), 