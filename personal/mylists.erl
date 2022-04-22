% Custome list module.
-module(mylists).
-export([map/2, sum/1]).

% Returns mapped list of values based on the given fun.
%map(_, []) -> [];

%map (F, [H | T])
%	->  [F(H) | map(F, T)].

% Map function with list comprehension.
map(F, L)
	-> [F(X) || X <- L].

% Return the sum of list elements.
sum([H | T])
	-> H + sum(T);
	
sum([]) -> 0.