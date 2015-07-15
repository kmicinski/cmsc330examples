%% Increment, X is the argument, Y is return value
increment(X,Y) :-
    Y is X+1.

addN(X,0,X).
addN(X,N,Y) :-
    X1 is X+1,
    N1 is N-1,
    addN(X1,N1,Y).

father(andrew,sam).

concat([],L2,L2).
concat([E|L1],L2,[E|C]) :-
    concat(L1,L2,C).

    


