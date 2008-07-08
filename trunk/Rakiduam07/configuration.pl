:- module(configuration,_,_).
%:- module(configuration,[get_element/2,get_element_number/2,get_player/3,get_attributes/2],_).

:- use_package(xml_path).
:- use_package(pillow).
:- use_module(library(file_utils)).

:- initialization(start).


xml_file('config.xml').


fetch_xml(Terms) :-
	xml_file(Xml),
	file_to_string(Xml,Content),
	xml2terms(Content,Terms).

get_element_number(Terms,Clave,Valor):-
	member(env(config,[],L),Terms),
	member(env(Clave,[],[ValorS]),L),
	number_codes(Valor,ValorS).

% get_attributes(Terms,Clave,List):-
% 	member(env(config,[],L),Terms),	
% 	member(env(Clave,List1,_),L),
% 	convert(List1,List).

% convert([],[]).
% convert([_A=V|R],[VN|S]):-
% 	number_codes(VN,V),
% 	convert(R,S).

get_element(Terms,Clave,Valor):-
%	Query = config::(Clave)::(Valor),
%	xml_query(Query, Term).
	member(env(config,[],L),Terms),
	member(env(Clave,[],[ValorS]),L),
	atom_codes(Valor,ValorS).	


get_players(Terms,Nombre,Jug,Equipo):-
	Query = config::players::player@(val(number,Jugs),val(team,Equipos))::(Nombres),
	xml_query(Query, Terms),
 	atom_codes(Nombre,Nombres),
 	number_codes(Jug,Jugs),
 	atom_codes(Equipo,Equipos).

get_host_and_port(Terms,Element,Hostname,Port):-
	Query = config::(Element)@(val(hostname,HostnameS),val(port,PortS)),
	xml_query(Query, Terms),
 	atom_codes(Hostname,HostnameS),
 	number_codes(Port,PortS).


get_coord(Terms,Elem,X1,Y1,X2,Y2):-
	Query = config::(Elem)@(val('X1',X1s),val('X2',X2s),val('Y1',Y1s),val('Y2',Y2s)),
	xml_query(Query, Terms),
	number_codes(X1,X1s),	
	number_codes(X2,X2s),	
	number_codes(Y1,Y1s),	
	number_codes(Y2,Y2s).	

start:-
	fetch_xml(Terms),
	get_element_number(Terms,anchoArea,VanchoArea),
	asserta_fact(get_anchoArea(VanchoArea)),
	get_element_number(Terms,altoArea,ValtoArea),
	asserta_fact(get_altoArea(ValtoArea)),
	get_element_number(Terms,anchoAreaChica,VanchoAreaChica),
	asserta_fact(get_anchoAreaChica(VanchoAreaChica)),
	get_element_number(Terms,altoAreaChica,ValtoAreaChica),
	asserta_fact(get_altoAreaChica(ValtoAreaChica)),
	get_coord(Terms,arco_alto,X1aa,Y1aa,X2aa,Y2aa),
	asserta_fact(get_arco_alto(X1aa,Y1aa,X2aa,Y2aa)),
	get_coord(Terms,arco_bajo,X1ab,Y1ab,X2ab,Y2ab),
	asserta_fact(get_arco_bajo(X1ab,Y1ab,X2ab,Y2ab)),
	get_coord(Terms,field,X1f,Y1f,X2f,Y2f),
	asserta_fact(get_field(X1f,Y1f,X2f,Y2f)),
	get_element(Terms,environment,Venvironment),
	asserta_fact(get_environment(Venvironment)),
	get_host_and_port(Terms,video_server,VideoHost,VideoPort),
	asserta_fact(get_video_host(VideoHost)),
	asserta_fact(get_video_port(VideoPort)),
	get_host_and_port(Terms,command_server,VideoHost,VideoPort),
	asserta_fact(get_command_host(VideoHost)),
	asserta_fact(get_command_port(VideoPort)),
        assert_players(Terms).

assert_players(Terms):-
	get_players(Terms,Nombre,Jug,Equipo),
	assertz_fact(get_player(Nombre,Jug,Equipo)),
	fail.
assert_players(_).

	

:- data [
	get_anchoArea/1,
	get_altoArea/1,
	get_anchoAreaChica/1,
	get_altoAreaChica/1,
	get_arco_alto/4,
	get_arco_bajo/4,
	get_field/4,
	get_environment/1,
	get_video_host/1,
	get_video_port/1,
	get_command_host/1,
	get_command_port/1,
	get_numplayers/1,
	get_player/3
	].

% <config>
%   <anchoArea>14</anchoArea>
%   <altoArea>31</altoArea>
%   <anchoAreaChica>6</anchoAreaChica>
%   <altoAreaChica>15.5</altoAreaChica>
%   <!-- <accionesPrev>[0,0,0,0,0,0,0,0,0,0]</accionesPrev> -->
%   <!-- <accionesProm>[0,0,0,0,0,0,0,0,0,0]</accionesProm> -->
%   <arco_alto X1="93.4259" Y1="33.9320" X2="93.4259" Y2="49.6801"> </arco_alto>
%   <arco_bajo X1="6.8118" Y1="33.9320" X2="6.8118" Y2="49.6801">  </arco_bajo>
%   <field     X1="6.8118" Y1="6.3730" X2="93.4259" Y2="77.2392">  </field>
%   <environment>simulado</environment>
%   <host>127.0.0.1</host>
%   <port>6363</port>
%   <numplayers>3</numplayers>
%   <players>
%     <player  number="1" team="propio" >spot1</player>
%     <player  number="2" team="propio" >spot2</player>
%     <player  number="3" team="propio" >spot3</player>
%     <player  number="1" team="contrario" >spot4</player>
%     <player  number="2" team="contrario" >spot5</player>
%     <player  number="3" team="contrario" >car54</player>
%   </players>
% </config>

