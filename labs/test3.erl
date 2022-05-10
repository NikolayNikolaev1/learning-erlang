-module(test3).
-export([start/1, proc/4]).

proc(Shell, Tag, Sysproc, F) ->
	process_flag(trap_exit, Sysproc),
	F(),
	receive
		Msg ->
			io:format("Proc ~p get msg: ~p~n", [Tag, Msg])
	after 5000 ->
		      Shell ! {hello_from, Tag, self()}
	end.

start(Sysproc) ->
	process_flag(trap_exit, Sysproc),
	io:format("Shell Pid: ~p~n", [self()]),
	PidA = spawn_link(test3, proc, [self(), a, true, fun() -> ok end]),
	PidB = spawn_link(test3, proc, [self(), b, false, fun() -> exit(kill) end]),
	io:format("ProcA: ~p~nProcB: ~p~n", [PidA, PidB]).
