
%-------------------------------------------------
% my_takeout/3 - take out element from a list

my_takeout(E, [E|T], T).
my_takeout(E, [H|T], [H|T2]) :-
	my_takeout(E, T, T2).
	
%-------------------------------------------------
% my_permutation/2 - find permutation of a list

my_permutation([], []).
my_permutation(L, [H|T]) :-
	my_takeout(H, L, R), my_permutation(R, T).

%-------------------------------------------------
% is_sorted/1 - whether list is sorted

is_sorted([]).
is_sorted([_]).
is_sorted([X1,X2|T]) :- X1 =< X2, is_sorted([X2|T]).

%-------------------------------------------------
% my_sort/2 - find all permutations, return those that are sorted 

my_sort(Xs,Ys) :-
  permutation(Xs,Ys),
  is_sorted(Ys).

%-------------------------------------------------
%%% Insertion sort

insert(X,[],[X]).
insert(X,[Y|Ys],[Y|Zs]) :- X > Y, insert(X,Ys,Zs).
insert(X,[Y|Ys],[X,Y|Ys]) :- Y >= X.

insertionsort([X|Xs],Ys) :- insertionsort(Xs,Zs), insert(X,Zs,Ys).
insertionsort([],[]).

%-------------------------------------------------
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

