-module(lab4_server).
-export([client/2, loop/1, rpc_receive/1, start/0]).

%% Server side.
loop(State) ->
	receive
		{Client, {calc, X}} -> Response = X*X,
				       Client ! {self(), Response};
		{Client, {calc, X, Y}} -> Response = X+Y,
					  Client ! {self(), Response};
		{Client, Request} -> Response = {Request},
				     Client ! {self(), Response};
		Any -> io:format("Any: ~p~n", [Any])
	end,

	NewState = State,
	loop(NewState).

rpc(Server, Request) ->
	SyncPr = spawn(server, rpc_receive, [Server]),
	io:format("SyncPr: ~p~n", [SyncPr]),
	Server ! {SyncPr, Request},
	io:format("Hello from client!~n").

rpc_receive(Server) ->
	receive
		{Server, Response} -> Response;
		Any -> io:format("Any from client: ~p~n", [Any])
	end.

start() -> spawn(lab4_server, loop, [{}]).
%% ---------------------------------------------------------------------
%% Client side.
client(Server, {calc, X}) ->
	rpc(Server, {calc, X});

client(Server, {calc, X, Y}) ->
	rpc(Server, {calc, X, Y});

client(Server, Request) ->
	rpc(Server, Request).
