% Server module for getting the area of rectangle, square and circle.
-module(geometry_server).
-export([area/1,
	 area/2,
	 loop/0,
	 rpc/2,
	 start/0]).

% Request function from client.
area(Pid, Shape)
	-> rpc(Pid, Shape).

% Area functions for diffrent shapes.
area({rectangle, Width, Height})
	-> Area = Width * Height,
	   io:format("Area of rectangle with Width: ~p and Height: ~p is ~p.~n", [Width, Height, Area]);

area({square, Width}) 
	-> Area = Width * Width,
	   io:format("Area of square with Width: ~p is ~p.~n", [Width, Area]);

area({circle, Radius})
	-> Area = 3.14159 * Radius * Radius,
	   io:format("Area of circle with radius: ~p is ~p.~n", [Radius, Area]).

% Process for getting the area of a shape, based on the message input.
loop() ->
	receive
		{From, {rectangle, Width, Height}}	-> From ! {self(), area({rectangle, Width, Height})};
		{From, {square, Width}}			-> From ! {self(), area({square, Width})};
		{From, {circle, Radius}}		-> From ! {self(), area({circle, Radius})};
		{From, Other}				-> From ! {self(), io:format("There is no area for ~p.~n", [Other])}
	end,
	loop().

% Remote procedure call.
rpc(Pid, Request) ->
	Pid ! {self(), Request},
	receive
		{Pid, Response} -> Response
	end.

% Start the server.
start() -> spawn(fun loop/0).
