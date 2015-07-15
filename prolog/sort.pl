%% Sorting by specification; inefficient!
% Exponential running time. Try: mysort([3,2,1,2,7,6,9,10,4,7],X).



% sort(SortedFormOfList, List).

is_sorted([]).
is_sorted([_]).
is_sorted([X1,X2|T]) :- X1 =< X2, is_sorted([X2|T]).

remove(X,[X|L],L).
remove(X,[H|L],[H|M]) :- remove(X,L,M).

permutation([], []).
permutation(L, [X|Xs]) :- remove(X, L, Rest), permutation(Rest, Xs).

mysort(Xs,Ys) :-
  permutation(Xs,Ys),
  is_sorted(Ys).

%%% Insertion sort

insert(X,[],[X]).
insert(X,[Y|Ys],[Y|Zs]) :- X > Y, insert(X,Ys,Zs).
insert(X,[Y|Ys],[X,Y|Ys]) :- Y >= X.

insertionsort([X|Xs],Ys) :- insertionsort(Xs,Zs), insert(X,Zs,Ys).
insertionsort([],[]).

%%% Quicksort

append([X|Xs],Ys,[X|Zs]) :- append(Xs,Ys,Zs).
append([],Ys,Ys).

partition([X|Xs],Y,[X|Ls],Bs) :-
  Y >= X, partition(Xs,Y,Ls,Bs).
partition([X|Xs],Y,Ls,[X|Bs]) :-
  X > Y, partition(Xs,Y,Ls,Bs).
partition([],Y,[],[]).

quicksort([X|Xs],Ys) :-
  partition(Xs,X,Littles,Bigs),
  quicksort(Littles,Ls),
  quicksort(Bigs,Bs),
  append(Ls,[X|Bs],Ys).
quicksort([],[]).

merge([X|Xs],[Y|Ys],[X|Zs]) :- X < Y, merge(Xs, [Y|Ys], Zs).
merge([X|Xs],[Y|Ys],[X,Y|Zs]) :- X = Y, merge(Xs, Ys, Zs).
merge([X|Xs],[Y|Ys],[Y|Zs]) :- X > Y, merge([X|Xs], Ys, Zs).
merge(Xs,[],Xs).
merge([],Ys,Ys).

/*
merge([X|Xs],[Y|Ys],[X|Zs]) :- X < Y, !, merge(Xs, [Y|Ys], Zs).
merge([X|Xs],[Y|Ys],[X,Y|Zs]) :- X = Y, !, merge(Xs, Ys, Zs).
merge([X|Xs],[Y|Ys],[Y|Zs]) :- X > Y, !, merge([X|Xs], Ys, Zs).
merge(Xs,[],Xs) :- !.
merge([],Ys,Ys) :- !.
*/
