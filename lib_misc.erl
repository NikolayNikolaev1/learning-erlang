% Recreation of the lib_misc module.
-module(lib_misc).
-export([for/2, sum/1]).

% For loop.
for(Max, Max, F)
	-> [F(Max)];

for(I, Max, F)
	-> [F(I)|for(I+1, Max, F)].

% Sum a list of numbers and return the result.
sum(Num)
	-> sum(Num, 0).
	
sum([], Num)
	-> Num;

sum([H|T], Num)
	-> sum(T, H + Num).