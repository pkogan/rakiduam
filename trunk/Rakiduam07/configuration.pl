:- module(configuration,_,_).
%:- module(configuration,[get_element/2,get_element_number/2,get_player/3,get_attributes/2],_).

:- use_package(xml_path).
:- use_package(pillow).
:- use_module(library(file_utils)).


xml_file('config.xml').


fetch_xml(Terms) :-
	xml_file(Xml),
	file_to_string(Xml,Content),
	xml2terms(Content,Terms).

get_element_number(Clave,Valor):-
	fetch_xml(Terms),
	member(env(config,[],L),Terms),
	member(env(Clave,[],[ValorS]),L),
	number_codes(Valor,ValorS).

get_attributes(Clave,List):-
	fetch_xml(Term),
	member(env(config,[],L),Term),	
	member(env(Clave,List1,_),L),
	convert(List1,List).

convert([],[]).
convert([_A=V|R],[VN|S]):-
	number_codes(VN,V),
	convert(R,S).

get_element(Clave,Valor):-
	fetch_xml(Term),
	member(env(config,[],L),Term),
	member(env(Clave,[],[ValorS]),L),
	atom_codes(Valor,ValorS).

get_player(Nombre,Jug,Equipo):-
	fetch_xml(Terms),
	Query = config::players::player@(val(number,Jugs),val(team,Equipos))::(Nombres),
	xml_query(Query, Terms),
 	atom_codes(Nombre,Nombres),
 	number_codes(Jug,Jugs),
 	atom_codes(Equipo,Equipos).


