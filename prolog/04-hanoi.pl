%--------------------------------------------------------
% hello - read name from stdin, write hello name to stdout

hello :-
    read(X),
    write('Hello'), 
    tab(1),         % one space
    write(X).
	
%--------------------------------------------------------
% move/4 - solve Towers of Hanoi problem
%		move(size,from,dest,other)

move(1,X,Y,_) :-  
    write('Move top disk from '), 
    write(X), 
    write(' to '), 
    write(Y), 
    nl. 

move(N,X,Y,Z) :- 
    N>1, 
    M is N-1, 
    move(M,X,Z,Y), 
    move(1,X,Y,_), 
    move(M,Z,Y,X). 
	