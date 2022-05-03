-module(test1).
-export([start/1, proc/2]).

proc(Shell, Tag) ->
	receive
	after 5000 -> Shell ! {hello_from, Tag, self()}
	end.

start(Sysproc) ->
	process_flag(trap_exit, Sysproc),
	io:format("Shell Pid: ~p~n", [self()]),
	
	PidA = spawn_link(test1, proc, [self(), a]),
	PidB = spawn_link(test1, proc, [self(), b]),

	io:format("ProcA: ~p~nProcB: ~p~n", [PidA, PidB]),
	exit(PidB, kill).
