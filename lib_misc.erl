% Recreation of the lib_misc module.
-module(lib_misc).
-export([for/3, perms/1, pythag/1, qsort/1, sum/1]).

% For loop.
for(Max, Max, F)
	-> [F(Max)];

for(I, Max, F)
	-> [F(I)|for(I+1, Max, F)].

% Returns list of permutations from the given string.
perms([]) -> [[]];

perms(L) 
	-> [[H | T] || H <- L, T <- perms(L--[H])].

% Take all values of A from 1 to N, all values of B from 1 to N, and all values of C from 1 to N
% such that A + B + C is less than or equal to N and A*A + B*B = C*C.
pythag(N)
	-> [{A, B, C} ||
		A <- lists:seq(1, N),
		B <- lists:seq(1, N),
		C <- lists:seq(1, N),
		A + B + C =< N,
		A*A + B*B =:= C*C].

% Quicksorts elements in list from lowest to highest.
qsort([]) -> [];

qsort([Pivot | T])
	-> qsort([X || X <- T, X < Pivot])
		++ [Pivot] ++
		qsort([X || X <- T, X >= Pivot]).

% Sum a list of numbers and return the result.
sum(Num)
	-> sum(Num, 0).
	
sum([], Num)
	-> Num;

sum([H|T], Num)
	-> sum(T, H + Num).