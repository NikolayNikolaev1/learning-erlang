% Recreation of the lib_misc module.
-module(lib_misc).
-export([
		for/3,
		odds_and_evens/1,
		odds_and_evens_acc/1,
		odds_and_evens_acc/3,
		perms/1,
		pythag/1,
		qsort/1,
		sqrt/1,
		sum/1
]).

% For loop.
for(Max, Max, F)
	-> [F(Max)];

for(I, Max, F)
	-> [F(I)|for(I+1, Max, F)].

% Splits a list of numbers into 2 lists - even and odd numbers, by traversing the list 2 times.
odds_and_evens(Numbers) ->
		Even =  [Num || Num <- Numbers, Num rem 2 =:= 0],
		Odd = [Num || Num <- Numbers, Num rem 2 =:= 1],
		{Even, Odd}.

% odds_and_evens with accumolator.
odds_and_evens_acc(Numbers) ->
		odds_and_evens_acc(Numbers, [], []).

odds_and_evens_acc([H | T], Odd, Even) ->
		case H rem 2 of
				0		-> odds_and_evens_acc(T, Odd, [H | Even]);
				1		-> odds_and_evens_acc(T, [H | Odd], Even)
		end;
		
odds_and_evens_acc([], Odd, Even) ->
		{Odd, Even}.

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

% Returns square root of given number with error handling message for negative numbers.
sqrt(Num) when Num < 0 ->
		erlang:error({squareRootNegativeArgument, Num});

sqrt(Num) ->
		math:sqrt(Num).

% Sum a list of numbers and return the result.
sum(Num)
	-> sum(Num, 0).
	
sum([], Num)
	-> Num;

sum([H|T], Num)
	-> sum(T, H + Num).