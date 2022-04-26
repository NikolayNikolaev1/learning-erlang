%% Lab for exercising work with 'fun's.
-module(lab5_fun).
-export([func_name/1, loop/1, rpc/2, start/1]).

func_name(0) -> ok;

func_name({F}) -> F(10);

func_name(_X) ->
	F = fun(1) -> 1;
	       ([_Y]) -> 0;
	       (Y) -> Y
	    end,
	F.
%%----------------------------------------------------------------------------------------------------
%% Process for dynamic functionality, changed from request.
loop(State) ->
	receive
		{Client, {change_functionality, NewF}} -> NewState = {my_functionality, NewF},
							  Client ! {self(), functionality_changed};
		{Client, Request} -> {my_functionality, F} = State,
				     Response = F(Request),
				     Client ! {self(), Response},
				     NewState = State;
		Any -> io:format("Any: ~p~n", [Any]),
		       NewState = State
	end,

	loop(NewState).

rpc(Server, Request) ->
	Server ! {self(), Request},

	receive
		{Server, Response} -> Response
	end.

start(F) -> spawn(server, loop, [{my_functionality, F}]).
