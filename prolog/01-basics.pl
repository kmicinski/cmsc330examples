%--------------------------------------------------------
% some family facts

woman(alice).
man(bob).
man(charlie).
man(dennis).

father(bob, charlie).
father(dennis, bob).
mother(alice, charlie).

% some family rules

son(X, Y) :- father(Y, X), man(X).
son(X, Y) :- mother(Y, X), man(X).

%--------------------------------------------------------

blonde(X) :-
	father(Father, X),
	blonde(Father),         % father is blond
	mother(Mother, X),
	blonde(Mother).         % mother is blond

blonde(alice).

%--------------------------------------------------------
% some Prolog clauses (facts)

bigger(horse, duck).
bigger(duck, gnat).

% some Prolog clauses (rules)

is_bigger(X,Y) :- bigger(X,Y).
is_bigger(X,Y) :- bigger(X,Z), is_bigger(Z,Y).

%--------------------------------------------------------
% Goal execution example

mortal(X) :- man(X).
mortal(X) :- woman(X).

man(socrates).
