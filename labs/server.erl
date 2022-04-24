-module(server).
-export([createChildren/2, loop/1, start/1, rpc/2]).

createChildren(Pid, Request) ->
	rpc(Pid, Request).

start({N, Parent}) ->
	spawn(server, loop, [{N, Parent, N}]).

loop({0, Parent, _ProcCount}) -> 
	Parent ! {children, [self()]};

loop({N, Parent, ProcCount}) ->
	receive
		create_child -> Child = spawn(server, loop, [{N-1, self(), ProcCount}]),
				io:format("N:~p, Child:~p~n", [N, Child]),
				Child ! create_child;
		{do, X} -> M = X * X,
			   io:format("Me:~p, N:~p, M:~p~n", [self(), N, M]),
			   Parent ! {do, M};
		{children, ChildList} when N =:= ProcCount -> Parent ! ChildList;
		{children, ChildList} -> Parent ! {children, [self() | ChildList]};
		Any -> Any
	end,
	loop({N, Parent, ProcCount}).

rpc(Pid, Request) ->
	Pid ! Request,
	receive
		Response ->
			Response
	end.
