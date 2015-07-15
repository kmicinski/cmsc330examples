/* Merge two sorted lists X,Y into one sorted list Z */
merge([X|Xs], [Y|Ys], [X|Zs]) :-
    X < Y,
    merge(Xs, [Y|Ys], Zs).

merge([X|Xs], [Y|Ys], [X,Y|Zs]) :-
    X =:= Y,
    merge(Xs,Ys,Zs).

merge([X|Xs], [Y|Ys], [Y|Zs]) :-
    X > Y,
    merge([X|Xs],Ys,Zs).

merge(Xs, [], Xs) :- !.
merge([], Ys, Ys) :- !.


/* Merge two sorted lists X,Y into one sorted list Z -- with cut*/
/*
merge([X|Xs], [Y|Ys], [X|Zs]) :-
    X < Y, !,
    merge(Xs, [Y|Ys], Zs).

merge([X|Xs], [Y|Ys], [X,Y|Zs]) :-
    X =:= Y, !,
    merge(Xs,Ys,Zs).

merge([X|Xs], [Y|Ys], [Y|Zs]) :-
    X > Y, !,
    merge([X|Xs],Ys,Zs).

merge(Xs, [], Xs) :- !.
merge([], Ys, Ys) :- !.

*/

/* Eliminating redundancy */

my_sort(Xs, Ys) :-
    append(As, [X,Y|Bs], Xs),
    X > Y,
    append(As, [Y,X|Bs], Xs1),
    my_sort(Xs1, Ys).

my_sort(Xs, Xs) :-
    ordered(Xs).

ordered([_]).
ordered([X,Y|Ys]) :-
    X =< Y,
    ordered([Y|Ys]).

/* sort without the redundancy */

nr_sort(Xs, Ys) :-
    append(As, [X,Y|Bs], Xs),
    X > Y,
    !,
    append(As, [Y,X|Bs], Xs1),
    nr_sort(Xs1, Ys).

nr_sort(Xs, Xs) :-
    ordered(Xs),
    !.

/* red cuts */

if_then_else(P,Q,_) :- P, !, Q.
if_then_else(_,_,R) :- R.

/* Alternatively, you could have:
      if_then_else(P,Q,R) :- not(P), R.
   as the second rule, but this is computationally Expensive */


/* Implementing not */
my_not(X) :- X, !, fail.
my_not(_).



/* Using not */

my_flatten([],[]).
my_flatten(X,[X]) :- \+ is_list(X).
my_flatten([X|Xs],Zs) :-
    my_flatten(X,Y),
    my_flatten(Xs,Ys),
    append(Y,Ys,Zs).

completely_flatten(L,F) :-
    my_flatten(L,L),
    F = L.
completely_flatten(L,F) :-
    my_flatten(L, X), !,
    completely_flatten(X,F).

has_factor(N,L) :- 
    N mod L =:= 0.
has_factor(N,L) :-
    L * L < N,
    L2 is L + 2,
    has_factor(N, L2).

is_prime(2).
is_prime(3).
is_prime(P) :-
    integer(P),
    2 =< P,     % note the direction!
    P mod 2 =\= 0,
    \+ has_factor(P, 3).


/* Why you should be careful when using not */

unmarried_student(X) :-
    not(married(X)),
    student(X).

student(bill).
student(steve).
married(joe).

% What will unmarried_student(X) return?


/* Tail recursion */

ntr_reverse([ ],[ ]).
ntr_reverse([X|L],Rev) :- ntr_reverse(L,RL), append(RL,[X],Rev).

tr_reverse([ ],[ ]).
tr_reverse(L,RL) :- tr_reverse(L,[ ],RL).

tr_reverse([ ],RL,RL).
tr_reverse([X|L],PRL,RL) :- tr_reverse(L,[X|PRL],RL).

/*
 * Sledgehammer approach to ensuring no backtracking:
 * A :- B1,...,Bn,Bn1.
 * A :- B1,...,Bn,!,Bn1.
 */

