:- module(soccer_strips,['<-'/2,achieves/2,preconditions/2,deletes/2,holds/2,primitive/1],[]).

:- op(1200,xfx,[<-]).
:- data holds/2.



% ACTIONS
% move(Ag,Pos,Pos_1) is the action of Ag moving from Pos to Pos_1
preconditions(move(Ag,Pos,Pos_1),
    [player(Ag), waiting_at(Ag,Pos), adjacent(Pos,Pos_1)]).

%una posible mejora es hacer waiting para Ag, y ball_at para Obj
%hacer que kick patee a otra posicion.
preconditions(grabBall(Ag,Obj,Pos),
    [player(Ag), Ag \= Obj, waiting_at(Obj,Pos), at(Ag,Pos) ]).
% putdown(Ag,Obj,Pos)
preconditions(kick(Ag,Obj,From,To), 
    [player(Ag),  Ag \= Obj, at(Ag,From), carrying(Ag,Obj), inReach(From,To)]).
% unlock(Ag,Door)

% move(Ag,Pos,Pos_1) is the action of Ag moving from Pos to Pos_1
achieves(move(Ag,_Pos,Pos_1),waiting_at(Ag,Pos_1)).
% grabBall(Ag,Obj,Pos) is the action of agent Ag picking up Obj.
achieves(grabBall(Ag,Obj,_Pos), carrying(Ag,Obj)).
% kick(Ag,Obj,Pos)
achieves(kick(_Ag,Obj,_From,To),waiting_at(Obj,To)).

achieves(init,X) :-
   holds(X,init).



% move(Ag,Pos,Pos_1) is the action of Ag moving from Pos to Pos_1
deletes(move(Ag,Pos,_Pos_1),waiting_at(Ag,Pos)).
% grabBall(Ag,Obj,Pos) is the action of agent Ag picking up Obj.
deletes(grabBall(_Ag,Obj,Pos), waiting_at(Obj,Pos)).
% kick(Ag,Obj,Pos)
deletes(kick(Ag,Obj,_From,_To),carrying(Ag,Obj)).


% PRIMITIVE RELATIONS
primitive(carrying(_,_)).
primitive(waiting_at(_,_)).

% DERIVED RELATIONS

at(Obj,Pos) <-
   [waiting_at(Obj,Pos)].
at(Obj,Pos) <-
   [player(Ag), Ag \= Obj, carrying(Ag,Obj), at(Ag,Pos)].


% +---------+---------+---------+
% |cell(1,3)|cell(2,3)|cell(3,3)|
% |         |         |         |
% |         |         |         |
% |         |         |         |
% |         |         |         |
% +---------+---------+---------+
% |cell(1,2)|cell(2,2)|cell(3,2)|
% |         |         |         |
% |         |         |         |
% |         |         |         |
% |         |         |         |
% +---------+---------+---------+
% |cell(1,1)|cell(2,1)|cell(3,1)|
% |         |         |         |
% |         |         |         |
% |         |         |         |
% |         |         |         |
% +---------+---------+---------+



% adjacent(cell(C,R1),cell(C,R2)) <- [1 is abs(R1-R2)].
% adjacent(cell(C1,R),cell(C2,R)) <- [1 is abs(C1-C2)].

%columna 1
adjacent(cell(1,1),cell(1,2)) <- [].
adjacent(cell(1,1),cell(2,1)) <- [].

adjacent(cell(1,2),cell(1,1)) <- [].
adjacent(cell(1,2),cell(1,3)) <- [].
adjacent(cell(1,2),cell(2,2)) <- [].

adjacent(cell(1,3),cell(1,2)) <- [].
adjacent(cell(1,3),cell(2,3)) <- [].
%columna 2
adjacent(cell(2,1),cell(3,1)) <- [].
adjacent(cell(2,1),cell(2,2)) <- [].
adjacent(cell(2,1),cell(1,1)) <- [].

adjacent(cell(2,2),cell(3,2)) <- [].
adjacent(cell(2,2),cell(2,3)) <- [].
adjacent(cell(2,2),cell(2,1)) <- [].
adjacent(cell(2,2),cell(1,2)) <- [].

adjacent(cell(2,3),cell(3,3)) <- [].
adjacent(cell(2,3),cell(1,3)) <- [].
adjacent(cell(2,3),cell(2,2)) <- [].

%columna3
adjacent(cell(3,1),cell(3,2)) <- [].
adjacent(cell(3,1),cell(2,1)) <- [].

adjacent(cell(3,2),cell(3,3)) <- [].
adjacent(cell(3,2),cell(3,1)) <- [].
adjacent(cell(3,2),cell(2,2)) <- [].

adjacent(cell(3,3),cell(3,2)) <- [].
adjacent(cell(3,3),cell(2,3)) <- [].


% adjacent(field1,field2) <- [].
% adjacent(field1,field4) <- [].
% adjacent(field2,field3) <- [].
% adjacent(field2,field1) <- [].
% adjacent(field2,field5) <- [].
% adjacent(field3,field2) <- [].
% adjacent(field3,field6) <- [].
% adjacent(field4,field1) <- [].
% adjacent(field4,field7) <- [].
% adjacent(field4,field5) <- [].
% adjacent(field5,field2) <- [].
% adjacent(field5,field4) <- [].
% adjacent(field5,field6) <- [].
% adjacent(field5,field7) <- [].
% adjacent(field5,field8) <- [].
% adjacent(field6,field3) <- [].
% adjacent(field6,field5) <- [].
% adjacent(field6,field8) <- [].
% adjacent(field7,field4) <- [].
% adjacent(field7,field5) <- [].
% adjacent(field7,field8) <- [].
% adjacent(field8,field5) <- [].
% adjacent(field8,field6) <- [].
% adjacent(field8,field7) <- [].

player(kula) <- [].

% inReach(cell(2,_),cell(3,_)) <- [].
% inReach(cell(2,_),cell(1,_)) <- [].
% inReach(cell(3,_),cell(2,_)) <- [].
inReach(cell(1,_),oppGoal) <- [].


% inReach(field5,field6) <- [].
% inReach(field4,field7) <- [].
% inReach(field7,oppGoal) <- [].
% inReach(field6,oppGoal) <- [].


% INITIAL SITUATION
holds(waiting_at(kula,cell(3,3)),init).
holds(waiting_at(ball,cell(2,2)),init).


