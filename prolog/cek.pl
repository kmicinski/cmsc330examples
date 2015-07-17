%% 
%% The CEK machine in Prolog
%%

%% 
%% Environments
%%

% Environments are association lists
empty_env([]).

% Adding to environment is concatenation
add(K,V,E,[[K,V]|E]).

% Looking up environment is matching
lookup(K,[[K,V]|_],V).
lookup(K,[[K1,V]|T],V1) :- K1 \= K, lookup(K,T,V1).

%% 
%% Terms
%% 

%% Terms
%% terms are...
%% lam(x,T)    -- \x. t
%% app(T1,T2)  -- t1 t2
%% var(x)      -- x

%% 
%% Step relation
%%

%% Application
step([app(T1,T2),E,K],[T1,E,earg(T2,E,K)]).

%% Lookup variable and dereference closure in environment
step([var(X),E,K],[L,E1,K]) :- lookup(X,E,[L,E1]).

%% Finish evaluating a function, start evaluating an argument
step([lam(X,T1),E1,earg(T2,E2,K)],[T2,E2,ecall(lam(X,T1),E1,K)]).

%% Finish evaluating an argument, call function
step([lam(X,T1),E1,ecall(lam(X2,T2),E2,K)],[T2,E3,K]) :-
    add(X2,[lam(X,T1),E1],E2,E3).

%% Computing

%% Initial term is machine M
initial(T,[T,[],done]).

%% A state is final when...
final([lam(_,_),_,done]).

%% Compute...

%% Tail recursive...
results_in(M1,M1) :- final(M1),!.
results_in(M1,M3) :- step(M1,M2),results_in(M2,M3).

compute(T1,T2) :- initial(T1,S1),results_in(S1,S2),S2 = [T2,_,_].

%% (\f. (f (\x. x))) (\g. g (\y. y))

%% compute(app(lam(f,app(var(f),lam(x,var(x)))),lam(g,app(var(g),lam(y,y)))),X).
%% results in ... 
%% (\y. y)

%% compute(app(app(lam(x,lam(y,var(x))),(lam(x,var(x)))),lam(y,var(y))),X).
%% results in ...
%% X = lam(x, var(x)) .
