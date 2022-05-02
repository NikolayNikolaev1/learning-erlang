%% Client-Serve game based on Bulls and Cows.
-module(bulls_and_cows).
-export([client/2, rpc/2, loop/1, start/0]).

%% Generate a random valid code of 4 numbers with unique digits from 0 to 9.
create_code() -> 
	create_code([], 4, lists:seq(0, 9)).

create_code(SecretCode, 0, _Digits) -> SecretCode;

create_code(SecretCode, N, Digits) ->
	Next = lists:nth(rand:uniform(length(Digits)), Digits),
	create_code([Next | SecretCode], N-1, Digits -- [Next]).

%% Returns the result of the current user inputed code.
get_result(_SecretCode, [], Bulls, Cows, 5) -> {Bulls, Cows};

get_result(SecretCode, SecretCode, _Bulls, _Cows, _Counter) -> {4, 0};

get_result(SecretCode, [Next|ClientCode], Bulls, Cows, Counter) ->
	case lists:nth(Counter, SecretCode) =:= Next of
		true -> get_result(SecretCode, ClientCode, Bulls+1, Cows, Counter+1);
		false -> NewCows = [0|[1 || X <- SecretCode, X =:= Next]],
			 get_result(SecretCode, ClientCode, Bulls, Cows+lists:sum(NewCows), Counter+1)
	end.

%% TODO: Working on validation of user input.
%is_valid(ClientCode) ->length(ClientCode) =:= 4 and [|| X <- ClientCode]

loop({SecretCode}) ->
	State = {SecretCode},

	receive
		{Client, new_game} -> 
			NewCode = create_code(),
			NewState = {NewCode},
			Client ! {self(), new_game};
		{Client, {try_code, ClientCode}} -> Result = get_result(SecretCode, ClientCode, 0, 0, 1),
						    Client ! {self(), Result},
						    NewState = {SecretCode};
		Any -> Any,
		       NewState = State
	end,

	loop(NewState).

rpc(Server, Request) ->
	Server ! {self(), Request},
	
	receive
		{Server, new_game} -> 
			io:format("New Game started.~n");
		{Server, {Bulls, Cows}} ->
			io:format("Bulls: ~p, Cows: ~p~n", [Bulls, Cows]);
		{Server, Response} -> Response;
		Any -> io:format("Any: ~p~n", [Any])
	end.

start() ->
	RandomCode = create_code(),
	spawn(bulls_and_cows, loop, [{RandomCode}]).

client(Server, Request) ->
	rpc(Server, Request).
