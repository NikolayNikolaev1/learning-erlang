-module(binary_tree).
-export([client/2, loop/1, rpc/2, start/1]).

%% Client commands to change ring server functionallity.
client(Server, {change_func, multiply_itself}) ->
	rpc(Server, {change_func, fun(X) -> X*X end});

client(Server, {change_func, multiplyby_two}) ->
	rpc(Server, {change_func, fun(X) -> X*2 end});

client(Server, Request) ->
	rpc(Server, Request).

loop({1, Root, LChild, RChild, {function, F, FuncResult}}) ->
	Level = 1,
	Func = {function, F, FuncResult},
	State = {Level, Root, LChild, RChild, Func},

	receive
		{Client, {change_func, NewFunc}} -> NewState = {Level, Root, LChild, RChild, {function, NewFunc, []}},
						    Root ! {Client, func_changed};
		create_children-> NewState = State;
		{Client, {exec_func, Input}} -> Result = F(Input),
						io:format("~p. Server ~p result from function: ~p.~n", [Level, self(), Result]),
						Root ! {Client, {func_result, Result}},
						NewState = State;
		Any -> Any,
		       NewState = State
	end,

	loop(NewState);

loop({Level, Root, LChild, RChild, {function, F, FuncResult}}) ->
	Func = {function, F, FuncResult},
	State = {Level, Root, LChild, RChild, Func},

	receive
		{Client, {change_func, NewFunc}} -> NewState = {Level, Root, LChild, RChild, {function, NewFunc, []}},
						    LChild ! {Client, {change_func, NewFunc}},
						    RChild ! {Client, {change_func, NewFunc}};
		create_children -> NewLChild = spawn(binary_tree, loop, [{Level-1, Root, [], [], Func}]),
				   NewRChild = spawn(binary_tree, loop, [{Level-1, Root, [], [], Func}]),
				   NewLChild ! create_children,
				   NewRChild ! create_children,
				   NewState = {Level, Root, NewLChild, NewRChild, Func};
		{Client, {exec_func, Input}} -> Result = F(Input),
						io:format("~p. Server ~p result from function: ~p.~n", [Level, self(), Result]),
						LChild ! {Client, {exec_func, Result}},
						RChild ! {Client, {exec_func, Result}},
						NewState = {Level, Root, LChild, RChild, {function, F, []}};
		{Client, func_changed} -> Client ! {self(), func_changed},
					  NewState = State;
		{Client, {func_result, Result}} when FuncResult =:= [] -> Response = {func_result, Result},
									  Client ! {self(), Response},
									  NewState = {Level, Root, LChild, RChild, {function, F, Result}};
		set_root -> NewState = {Level, self(), LChild, RChild, Func};
		Any -> Any,
		       NewState = State
	end,

	loop(NewState).

rpc(Server, Request) ->
	Server ! {self(), Request},

	receive
		{Server, func_changed} -> io:format("Response from server ~p: Function successfully changed.~n", [Server]);
		{Server, {func_result, Result}} -> io:format("Response from server ~p: ~p.~n", [Server, Result]);
		Any -> io:format("Any: ~p~n", [Any])
	end.

start({Size}) ->
	ServerPid = spawn(binary_tree, loop, [{Size, [], [], [], {function, fun(X) -> X*X end, []}}]),
	ServerPid ! set_root,
	ServerPid ! create_children,
	ServerPid.
