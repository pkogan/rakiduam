
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    % Copyright 2007 Pablo Kogan, Guillermo Torres, Mario Moya		    %
    % 									    %
    % This file is part of Rakiduam.					    %
    % 									    %
    % Rakiduam is free software; you can redistribute it and/or modify	    %
    % it under the terms of the GNU General Public License as published by  %
    % the Free Software Foundation; either version 3 of the License, or	    %
    % (at your option) any later version.				    %
    % 									    %
    % Rakiduam is distributed in the hope that it will be useful,	    %
    % but WITHOUT ANY WARRANTY; without even the implied warranty of	    %
    % MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the	    %
    % GNU General Public License for more details.			    %
    % 									    %
    % You should have received a copy of the GNU General Public License	    %
    % along with this program.  If not, see <http://www.gnu.org/licenses/>. %
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(perceptions,[assert_perceptions/0],[assertions]).

:-use_module(ambiente,[pelota_pred/3,jugador/1]).
:-use_module(configuration,[get_player/3,get_ball_name/1]).
:-use_module(field_grid,[subfield_center/4,point_to_cell_position/4]).

:- data [carrying/2,waiting_at/2].

:- comment(title, "Primitive perceptions module ").

:- comment(author, "Mario Moya").

:- comment(module, "Esté módulo resuelve e inserta las percepciones 
	primitivas que son necesarias para que el planificador las 
	entienda según su representacion en strips. En este caso las
	percepciones corresponden a las primitivas carrying y
	waiting_at.").



konstant(10).

assert_perceptions :-
	assert_carrying_ball,
	assert_waiting_at.

assert_carrying_ball :-
	pelota_pred(Xb,Yb,_),
	konstant(K),
	jugador(robot(propio,Number,pos(Xr,Yr))),
	Xr>Xb-K,
	Xr<Xb+K,
	Yr>Yb-K,
	Yr<Yb+K,
	get_ball_name(Ball),
	get_player(P,Number,propio),
	retractall_fact(carrying(_,_)),
	asserta_fact(carrying(P,Ball)).
	
assert_carrying_ball :-
	retractall_fact(carrying(_,_)).


assert_waiting_at :-
	get_player(P,Number,propio),
	jugador(robot(propio,Number,pos(Xr,Yr))),
	get_in_field_position(Xr,Yr,FieldCell),
	asserta_fact(waiting_at(P,FieldCell)),
	fail.

assert_waiting_at.

get_in_field_position(X,Y,cell(C1,R1)) :-
	point_to_cell_position(X,Y,C1,R1).
	

% jugador(robot(propio,1,pos(30,30))).
% jugador(robot(propio,2,pos(60,60))).
% jugador(robot(propio,3,pos(70,70))).


