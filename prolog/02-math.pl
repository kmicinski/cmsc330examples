%--------------------------------------------------
plus(X,Y,Z) :- Z is X+Y.

%--------------------------------------------------
factorial(0,1).
factorial(0,2).

factorial(N,F) :-  
	N > 0, 
	N1 is N-1,
	factorial(N1,F1),
	F is N*F1. 

%--------------------------------------------------
tail_factorial(0,F,F). 
tail_factorial(N,A,F) :-  
    N > 0, 
    A1 is N*A, 
    N1 is N-1, 
    tail_factorial(N1,A1,F). 

%--------------------------------------------------
factorial2(0,1).
factorial2(N,F) :-  
	N1 is N-1,
	factorial2(N1,F1),
	F is N*F1. 
		   
%--------------------------------------------------
tail_factorial2(0,F,F). 
tail_factorial2(N,A,F) :-  
    A1 is N*A, 
    N1 is N-1, 
    tail_factorial2(N1,A1,F). 
	
%--------------------------------------------------
reverse_factorial(0,1).
reverse_factorial(N,F) :- 
	reverse_factorial(N1,F1), 
	N is N1+1, 
	F is N*F1.
	
%--------------------------------------------------
fib(0,0).
fib(1,1).
fib(N, F) :-
	N >= 2,
	N1 is N-1, 
	N2 is N-2,
	fib(N1, F1),
	fib(N2, F2),
	F is F1+F2.

%--------------------------------------------------
fib2(0,0).
fib2(1,1).
fib2(N, F) :-
	N >= 2,
	fib_helper(N,0,1,F).

fib_helper(1,_,F,F).
fib_helper(N,F1,F2,F) :-
	F3 is F1+F2,
	N1 is N-1,
	fib_helper(N1,F2,F3,F).
	
	
  
