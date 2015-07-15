%-------------------------------------------------
% my_len/2 - find length of a list

my_len([],0).
my_len([_|T],F) :- 
	my_len(T,F1),
	F is F1+1.
	
%-------------------------------------------------
% my_concat/3 - concatenate two lists

my_concat([], L2, L2).
my_concat([Elmt | L1], L2, [Elmt | C]) :- 
	my_concat(L1, L2, C).

%-------------------------------------------------
% my_last/2 - find last element of a list

my_last([X], X).
my_last([_|T],X) :- my_last(T,X).

%-------------------------------------------------
% my_nth/3 - find nth element of a list 

my_nth(1, [H|_], H).
my_nth(N, [_|T], F) :-
    N1 is N-1,
    my_nth(N1, T, F).

%-------------------------------------------------
% my_rev/3 - find reverse of a list

my_rev(X,F) :- rev_helper(X,[],F).

rev_helper([],A,A).
rev_helper([H|T],A,F) :- 
	rev_helper(T,[H|A],F).

%-------------------------------------------------
% palindrome/1 - find whether list is a palindrome

palindrome(L) :- reverse(L, L).

%-------------------------------------------------
% my_takeout/3 - take out one occurrence of element E from list
%                starts with 1st occurence of E in list
%                backtracking removes 2nd occurence of E, then 3rd, etc.

my_takeout(E, [E|T], T).
my_takeout(E, [H|T], [H|T2]) :-
	my_takeout(E, T, T2).

%-------------------------------------------------
% my_takeout2/3 - take out one occurrence of element E from list
%                starts with last occurence of E in list
%                backtracking removes next to last occurence of E, etc.

my_takeout2(E, [H|T], [H|T2]) :-
	my_takeout2(E, T, T2).
my_takeout2(E, [E|T], T).

%-------------------------------------------------
% my_takeout_first/3 - take out only first occurrence of element from list

my_takeout_first(E, [E|T], T).
my_takeout_first(E, [H|T], [H|T2]) :-
	E \= H,
	my_takeout_first(E, T, T2).
	
% alternative implemention using cut

my_takeout_first2(E, [E|T], T) :- !.
my_takeout_first2(E, [H|T], [H|T2]) :-
	my_takeout_first2(E, T, T2).

%-------------------------------------------------
% my_takeout_all/3 - take out all occurrences of element from list 

my_takeout_all(_, [], []).
my_takeout_all(E, [E|T], T2) :-
	my_takeout_all(E, T, T2).
my_takeout_all(E, [H|T], [H|T2]) :-
	E \= H,
	my_takeout_all(E, T, T2).
	
%-------------------------------------------------
% my_permutation/2 - find permutation of a list

my_permutation([], []).
my_permutation(L, [H|T]) :-
	my_takeout(H, L, R),
	my_permutation(R, T).

