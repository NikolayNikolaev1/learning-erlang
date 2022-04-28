-module(bulls_and_cows).
-export([client/2, rpc/2, loop/1, start/0]).

createCode() -> 
	createCode([], 4, lists:seq(0, 9)).

createCode(SecretCode, 0, _Digits) -> SecretCode;

createCode(SecretCode, N, Digits) ->
	Next = lists:nth(rand:uniform(length(Digits)), Digits),
	createCode([Next | SecretCode], N-1, Digits -- [Next]).

loop({_SecretCode, _SecretCode, ClientPid, _Bulls, _Cows}) ->
	ClientPid ! {self(), true};

loop({SecretCode, UserCode, ClientPid, Bulls, Cows}) ->
	[Cows + 1 || X =:= Y, X <- SecretCode, Y <- UserCode],


loop({SecretCode, UserCode, ClientPid, _Bulls, _Cows}) ->
	State = {SecretCode, UserCode, ClientPid},

	receive
		{Client, new_game} -> NewCode = createCode(),
				      NewState = {NewCode, [], Client};
		{Client, {try_code, TestCode}} -> NewState = {SecretCode, TestCode, Client};
		Any -> Any,
		       NewState = State
	end,

	loop(NewState).

rpc(Server, Request) ->
	Server ! {self(), Request},
	
	receive
		{Server, true} -> io:format("Game won!");
		{Server, Response} -> Response;
		Any -> io:format("Any: ~p~n", [Any])
	end.

start() ->
	RandomCode = createCode(),
	spawn(bulls_and_cows, loop, [{RandomCode, [], self(), [], []}]).

client(Server, Request) ->
	rpc(Server, Request).
