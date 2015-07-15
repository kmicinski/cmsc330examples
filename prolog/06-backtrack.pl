%--------------------------------------------------------
% backtracking, cut, not 

jedi(luke).
jedi(yoda).
sith(vader).
sith(maul).

% backtracking finds all pairs 
fight1(X,Y) :- jedi(X), sith(Y).

% cut ! commits all choices for goals encountered before !
fight2(X,Y) :- jedi(X), !, sith(Y).
fight3(X,Y) :- jedi(X), sith(Y), !.

% can use ! to implement not
not(X) :- call(X), !, fail.
not(_).

% not means "not currently provable" 
true_jedi1(X) :- jedi(X), not(sith(X)).
true_jedi2(X) :- not(sith(X)), jedi(X).

% X \= Y is the same as not(X=Y)
help1(X,Y) :- jedi(X), jedi(Y).
help2(X,Y) :- jedi(X), jedi(Y), X \= Y. 
help3(X,Y) :- jedi(X), X \= Y, jedi(Y).
help4(X,Y) :- X \= Y, jedi(X), jedi(Y).