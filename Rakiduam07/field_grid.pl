%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%     Copyright 2007 Pablo Kogan, Guillermo Torres, Mario Moya
%
%     This file is part of Rakiduam.
%
%     Rakiduam is free software; you can redistribute it and/or modify
%     it under the terms of the GNU General Public License as published by
%     the Free Software Foundation; either version 3 of the License, or
%     (at your option) any later version.
%
%     Rakiduam is distributed in the hope that it will be useful,
%     but WITHOUT ANY WARRANTY; without even the implied warranty of
%     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%     GNU General Public License for more details.
%
%     You should have received a copy of the GNU General Public License
%     along with this program.  If not, see <http://www.gnu.org/licenses/>.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:-module(field_grid,[subfield_center/4,point_to_cell_position/4],[assertions]).
%:-use_module(configuration).
:-use_module(ambiente,[largo_cancha/1,ancho_cancha/1]).

:- comment(title, "Field grid module ").

:- comment(author, "Mario Moya").

:- comment(module, "Este módulo calcula los casilleros en que se divide
	la cancha, según los paramétros que se hayan especificado
	en la configuración. La idea de este módulo es tener tantas
	filas y columnas como se especifique. Se asume que los indices
	para filas y columnas son números enteros que comienzan en 1").

%no deberian ser hechos.
number_of_rows(3).
number_of_columns(3).
% largo_cancha(83).
% ancho_cancha(67).


:- pred cell_with(-Width) :: int 
 # "return in @var{Width} the width size of the cell".
cell_width(L):- 
	number_of_columns(0),
	largo_cancha(L).

cell_width(Cw):- 
	number_of_columns(N),
	largo_cancha(L),
	Cw is L//N.

cell_height(A):- 
	number_of_rows(0),
	ancho_cancha(A).

cell_height(Ch):- 
	number_of_rows(N),
	ancho_cancha(A),
	Ch is A//N.

cell_x_coordinate(Column, X) :-
	cell_width(Cw),
	X is ((Column-1)*Cw) + (Cw//2).

cell_y_coordinate(Row, Y) :-
	cell_height(Ch),
	number_of_rows(N),
	Y is ((N-Row)*Ch) + (Ch//2).

%dadas un columna y una fila, calcula el punto central de la celda.
subfield_center(Column,Row,X,Y):-
	cell_x_coordinate(Column,X),
	cell_x_coordinate(Row,Y).


%dadas dos coordenada X e Y, calcula en que subcampo se encuentra dicho punto.
point_to_cell_position(X,Y,Column,Row) :-
	cell_height(Ch),
	Row is (Y//Ch) + 1,
	cell_width(Cw),
	Column is X//Cw + 1.
