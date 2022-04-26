-module(webserver).
-export([client/2, loop/0, rpc/2, rpc_receive/1, start/0]).

loop() ->
	receive
		{Client, home} -> Response = "<!DOCTYPE html>
				    <head>
				    	<title>Home Page</title>
				    </head>
				    <body>
				    	<h1>Welcome to Home!</h1>
				    	<p>This is home page for erlang webserver.</p>
				    </body>
				    </html>",
				  Client ! {self(), Response};
		Any -> io:format("404 - Not Found: ~p~n", [Any])
	end,

	loop().

rpc(Server, Request) ->
	SyncPr = spawn(webserver, rpc_receive, [Server]),
	Server ! {SyncPr, Request}.

rpc_receive(Server) ->
	receive
		{Server, Response} -> io:format("~p~n", [Response]),
				      Response;
		Any -> Any
	end.

start() -> spawn(webserver, loop, []).

client(Server, home) ->
	rpc(Server, home);

client(Server, Request) ->
	rpc(Server, Request).
