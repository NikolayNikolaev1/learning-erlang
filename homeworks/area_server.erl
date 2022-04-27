%% Module for dynamic changing functionality of server that returns the area of current selected shape from client.
-module(area_server).
-export([client/2, loop/1, rpc/2, start/1]).

%% Server side.
loop({area_functionality, AreaFun}) ->
	State = {area_functionality, AreaFun},

	receive
		{Client, {change_shape, NewShapeAreaFun}} -> NewState = {area_functionality, NewShapeAreaFun},
							     Client ! {self(), shape_changed};
		{Client, Request} -> Response = AreaFun(Request),
				     Client ! {self(), Response},
				     NewState = State;
		Any -> io:format("~p~n", [Any]),
		       NewState = State
	end,

	loop(NewState).

rpc(Server, Request) ->
	Server ! {self(), Request},

	receive
		{Server, shape_changed} -> io:format("Area functionality successfully changed.~n");
		{Server, Response} -> io:format("Area: ~p~n", [Response]);
		Any -> io:format("~p~n", [Any])
	end.

start(AreaFun) -> spawn(area_server, loop, [{area_functionality, AreaFun}]).
%% ------------------------------------------------------------------------------
%% Client side.
%% Changing area functionality of server based on given shape and its properties.
client(Server, {change_shape, circle}) ->
	rpc(Server, {change_shape, fun({circle, Radius}) -> 3.14159 * Radius * Radius end});

client(Server, {change_shape, rectangle}) ->
	rpc(Server, {change_shape, fun({rectangle, Width, Height}) -> Width * Height end});

client(Server, {change_shape, square}) ->
	rpc(Server, {change_shape, fun({square, Side}) -> Side * Side end});

client(Server, Request) ->
	rpc(Server, Request).
