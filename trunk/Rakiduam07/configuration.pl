:- module(configuration,[get_element/2,get_element_number/2,get_player/3,get_attributes/2],_).

:- use_package(pillow).
:- use_module(library(file_utils)).

get_element_number(Clave,Valor):-
	file_to_string('config.xml',Xml),
	xml2terms(Xml,Term),
	member(env(config,[],L),Term),
	member(env(Clave,[],[ValorS]),L),
	number_codes(Valor,ValorS).

get_attributes(Clave,List):-
	file_to_string('config.xml',Xml),
	xml2terms(Xml,Term),
	member(env(config,[],L),Term),	
	member(env(Clave,List1,_),L),
	convert(List1,List).

convert([],[]).
convert([_A=V|R],[VN|S]):-
	number_codes(VN,V),
	convert(R,S).

get_element(Clave,Valor):-
	file_to_string('config.xml',Xml),
	xml2terms(Xml,Term),
	member(env(config,[],L),Term),
	member(env(Clave,[],[ValorS]),L),
	atom_codes(Valor,ValorS).

get_player(Nombre,Jug,Equipo):-
	file_to_string('config.xml',Xml),
	xml2terms(Xml,Term),
	member(env(config,[],L),Term),
	member(env(players,[],L2),L),
	atom_codes(Nombre,Nombres),
	member(env(player,[number=Jugs,equipo=Equipos],[Nombres]),L2),
	number_codes(Jug,Jugs),
	atom_codes(Equipo,Equipos).
