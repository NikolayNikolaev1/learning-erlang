%% Test module to see how much time it takes to create big numbe of processes.
-module(processes).
-export([for/3, max/1, wait/0]).

%% For loop.
for(N, N, F) -> [F()];
for(I, N, F) -> [F() | for(I + 1, N, F)].

%% Create N processes then destroy them.
max(N) ->
	Max = erlang:system_info(process_limit),
	io:format("Maximum allowed processes: ~p~n", [Max]),
	statistics(runtime),
	statistics(wall_clock),
	L = for(1, N, fun() -> spawn(fun() -> wait() end) end),
	{_, Time1} = statistics(runtime),
	{_, Time2} = statistics(wall_clock),
	lists:foreach(fun(Pid) -> Pid ! die end, L),
	U1 = Time1 * 1000 / N,
	U2 = Time2 * 1000 / N,
	io:format("Process spawn time=~p (~p) microseconds~n", [U1, U2]).

wait() ->
	receive
		die -> void
	end.
