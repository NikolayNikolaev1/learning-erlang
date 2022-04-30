%% Module for server from binary_tree node processes with changeable functionality.
-module(binary_tree).
-export([client/2, loop/1, rpc/2, rpc_receive/1 , start/1]).

%% Client commands to change ring server functionallity.
client(Server, {change_func, multiply_itself}) ->
	rpc(Server, {change_func, fun(X) -> X*X end});

client(Server, {change_func, multiplyby_two}) ->
	rpc(Server, {change_func, fun(X) -> X*2 end});

client(Server, Request) ->
	rpc(Server, Request).

%% Server side.
%% ----------------------------------------------------------------------------------------------
%% Processes of the last level children.
loop({1, Root, LChild, RChild, Func}) ->
	Level = 1,
	State = {Level, Root, LChild, RChild, Func},

	receive
		%% Send msg to Root that functionality is changed in the last nodes.
		{Client, {change_func, NewFunc}} -> NewState = {Level, Root, LChild, RChild, NewFunc},
						    Root ! {Client, func_changed};
		create_children-> NewState = State;
		%% Get the final result and sends it with the client Pid to the Root process.
		{Client, {exec_func, Input}} -> Result = Func(Input),
						io:format("~p. Server ~p result from function: ~p.~n", [Level, self(), Result]),
						Root ! {Client, {func_result, Result}},
						NewState = State;
		Any -> Any,
		       NewState = State
	end,

	loop(NewState);

loop({Level, Root, LChild, RChild, Func}) ->
	State = {Level, Root, LChild, RChild, Func},

	receive
		%% Request from Client.
		%% Gets new function and sends it to its children.
		{Client, {change_func, NewFunc}} -> NewState = {Level, Root, LChild, RChild, NewFunc},
						    LChild ! {Client, {change_func, NewFunc}},
						    RChild ! {Client, {change_func, NewFunc}};
		%% Creates left and right noded children with decremented level.
		%% Then LChild and RChild creates another pair of children.
		create_children -> NewLChild = spawn(binary_tree, loop, [{Level-1, Root, [], [], Func}]),
				   NewRChild = spawn(binary_tree, loop, [{Level-1, Root, [], [], Func}]),
				   NewLChild ! create_children,
				   NewRChild ! create_children,
				   NewState = {Level, Root, NewLChild, NewRChild, Func};
		%% Request from Client.
		%% Get result from current function and sends it to its children.
		{Client, {exec_func, Input}} -> Result = Func(Input),
						io:format("~p. Server ~p result from function: ~p.~n", [Level, self(), Result]),
						LChild ! {Client, {exec_func, Result}},
						RChild ! {Client, {exec_func, Result}},
						NewState = State;
		%% Gets msg from last node processes in binary tree that functionality is changed.
		%% Returns Response to Client that functionality is changed.
		{Client, func_changed} -> Client ! {self(), func_changed},
					  NewState = State;
		%% Gets result from last level processes of binary tree.
		%% Sends Response to the Client with result info.
		{Client, {func_result, Result}}  -> Response = {func_result, Result},
						    Client ! {self(), Response},
						    NewState = State;
		set_root -> NewState = {Level, self(), LChild, RChild, Func};
		Any -> Any,
		       NewState = State
	end,

	loop(NewState).

rpc(Server, Request) ->
	SyncPid = spawn(binary_tree, rpc_receive, [Server]),
	Server ! {SyncPid, Request}.

%% Async functionality to prevent race condition and spam messages from last created node processes in binary tree.
rpc_receive(Server) ->
	receive
		{Server, func_changed} -> io:format("Response from server ~p: Function successfully changed.~n", [Server]);
		{Server, {func_result, Result}} -> io:format("Response from server ~p: ~p.~n", [Server, Result]);
		Any -> io:format("Any: ~p~n", [Any])
	end.

start({Size}) ->
	ServerPid = spawn(binary_tree, loop, [{Size, [], [], [], fun(X) -> X*X end}]),
	ServerPid ! set_root,
	ServerPid ! create_children,
	ServerPid.
