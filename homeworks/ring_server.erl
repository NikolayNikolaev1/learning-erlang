-module(ring_server).
-export([client/2, loop/1, rpc/2, start/1]).

%% Client commands to change ring server functionallity.
client(Server, {change_func, multiply_itself}) ->
	rpc(Server, {change_func, fun(X) -> X*X end});

client(Server, {change_func, multiplyby_two}) ->
	rpc(Server, {change_func, fun(X) -> X*2 end});

client(Server, Request) ->
	rpc(Server, Request).

%% Server side
%% ------------------------------------------------------------------------------------------
loop({1, Root, Parent, Func}) ->
	N = 1,
	State = {N, Root, Parent, Func},

	receive
		%% Get Request from Client.
		{Client, {change_func, NewFunc}} -> NewState = {N, Root, Parent, NewFunc},
						    Parent ! {Client, {update_func, NewFunc}};
		%% Last created server returns its Pid to root.
		%% Purpose is for closing the ring.
		create_child -> Root ! {get_last_pid, self()},
				NewState = State;
		%% Get final result of the function.
		%% Return Response to Client.
		{Client, {do_func, Result}} -> Response = {func_result, Result},
					       Client ! {self(), Response},
					       NewState = State;
		%% Get Request from Client.
		%% Send the result from func to other servers.
		{Client, {exec_func, Input}} -> Result = Func(Input),
						io:format("~p, Func Result: ~p~n", [N, Result]),
						Parent ! {Client, {do_func, Result}},
						NewState = State;
		{Client, {update_func, _NewFunc}} -> Client ! {self(), func_changed},
						     NewState = State;
		Any -> Any,
		       NewState = State
	end,

	loop(NewState);

loop({N, Root, Parent, Func}) ->
	State = {N, Root, Parent, Func},

	receive
		%% Creates child server for current process.
		create_child -> Child = spawn(ring_server, loop, [{N-1, Root, self(), Func}]),
				io:format("~p. Child: ~p created.~n", [N-1, Child]),
				Child ! create_child,
				NewState = State;
		%% Execute function from all servers.
		{Client, {do_func, Input}} -> Result = Func(Input),
					      io:format("~p. Func Result: ~p~n", [N, Result]),
					      Parent ! {Client, {do_func, Result}},
					      NewState = State;
		%% Sets the given pid(from last created server) as parent.
		%% Purpose is for closing the ring.
		{set_parent, Pid} -> NewState = {N, Root, Pid, Func};
		%% Change current functionality for all servers.
		{Client, {update_func, NewFunc}} -> NewState = {N, Root, Parent, NewFunc},
						    Parent ! {Client, {update_func, NewFunc}};
		Any -> Any,
		       NewState = State
	end,

	loop(NewState).

rpc(Server, Request) ->
	Server ! {self(), Request},
	receive
		{Server, func_changed} -> io:format("Response from Server: ~p: Function changed successfully.~n", [Server]);
		{Server, {func_result, Result}} -> io:format("Response from Server ~p: Result from function is ~p.~n", [Server, Result]);
		Any -> Any,
		       io:format("Any: ~p~n", [Any])
	end.

%% Starts ring server and returns the Pid from the last created Node.
start({Size}) ->
	ServerPid = spawn(ring_server, loop, [{Size, self(), [], fun(X) -> X*X end}]),
	io:format("~p. Server: ~p created.~n", [Size, ServerPid]),
       	ServerPid ! create_child,
	
	receive
		%% Get Pid from last created server and send it to the first created one.
		%% Purpose is for closing the ring.
		{get_last_pid, Pid} -> ServerPid ! {set_parent, Pid},
				       Pid
	end.
