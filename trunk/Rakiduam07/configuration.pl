:- module(configuration,[get_key/2,get_key_number/2],_).

:- use_package(pillow).
:- use_module(library(file_utils)).

get_key_number(Clave,Valor):-
	file_to_string('config.xml',Xml),
	xml2terms(Xml,Term),
	member(env(config,[],L),Term),
	member(env(Clave,[],[ValorS]),L),
	number_codes(Valor,ValorS).


get_key(Clave,Valor):-
	file_to_string('config.xml',Xml),
	xml2terms(Xml,Term),
	member(env(config,[],L),Term),
	member(env(Clave,[],[ValorS]),L),
	atom_codes(Valor,ValorS).

