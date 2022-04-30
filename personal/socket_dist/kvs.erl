-module(kvs).
-export([lookup/1, loop/0, start/0, store/2]).

%% @spec kvs:lookup(Key) -> {ok, Value} | undefined
%% 	Look up the value of Key, and return {ok, Value} if there is a value
%% 	associated with Key; otherwise, return undefined.
lookup(Key) ->
	rpc({lookup, Key}).

%% @spec kvs:start() -> true
%% 	Start the server; this creates a server with the registered name kvs.
start() ->
	register(kvs, spawn(fun() -> loop() end)).

%% @spec kvs:store(Key, Value) -> true
%% 	Associate Key with Value.
store(Key, Value) ->
	rpc({store, Key, Value}).
%%--------------------------------------------------------------------------------
%% Server side.
loop() ->
	receive
		{From, {store, Key, Value}} -> 
			put(Key, {ok, Value}),
			From ! {kvs, true},
			loop();
		{From, {lookup, Key}} ->
			From ! {kvs, get(Key)},
			loop()
	end.

rpc(Q) ->
	kvs ! {self(), Q},
	receive
		{kvs, Reply} -> Reply
	end.
