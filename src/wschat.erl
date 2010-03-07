%% Author: e
%% Created: 19.02.2010
%% Description: TODO: Add description to wschat
-module(wschat).

%%
%% Include files
%%

%%
%% Some defines for comfortable work
%%
-define(ub(X), unicode:characters_to_binary(X)).

%%
%% Records
%%
-record(client,
	{
		pid,	%  Process handling websocket connection
		ref,	%  Ref to process for monitoring
		name=""	%  Client name
	}
).

%%
%% Exported Functions
%%
-export([stop/0, start/1]).

%%
%% API Functions
%%

%%
%% TODO: Add description of stop/function_arity
%%
start(Port) ->
	Conn_Manager = spawn(fun() -> conn_manager() end),
	io:fwrite("Conn Manager Pid: ~w~n", [Conn_Manager]),
	register(conn_mgr, Conn_Manager),
	misultin:start_link([
		{port, Port}, 
		{loop, fun(Req) -> handle_http(Req, Port) end}, 
		{ws_loop, fun(Ws) -> handle_websocket_start(Ws) end}
	]).

%%
%% TODO: Add description of stop/function_arity
%%
stop() ->
	conn_mgr ! {quit},
	misultin:stop().


%%
%% Local Functions
%%

%%
%% callback on request received
%%
handle_http(Req, Port) ->
	Meth = Req:get(method),
	% Host = Req:get(host),
	Path = Req:resource([lowercase, urldecode]),
	dispatch_http(Meth, Path, Req, Port).

%% 
%% Dispatch HTTP request 
%%
dispatch_http('GET', [], Req, Port) ->
	handle_http_index(Req, Port);

dispatch_http('GET', ["favicon.ico"], Req, _Port) ->
	Path = ["favicon.ico"],
	Req:respond(404, 
				[{"Content-Type", "text/html"}], 
				["File favicon /", Path, " not found"]
			   );
	
dispatch_http(_Meth, _Path, Req, _Port) ->
	Req:respond(404, [{"Content-Type", "text/html"}], 
		[
			"File /", string:join(_Path, "/"), " not found. <br /> \n\r",
			"Method: " , atom_to_list(_Meth) 
		]).

%%
%% Index page for chat
%%

handle_http_index(Req, Port) ->	
	% output
	io:fwrite("Sending index page...~n"),
	FileName = "www/index2.html",
	FileRes = file:read_file(FileName),
	case FileRes of
		{ok, Tmpl} ->
			Page = io_lib:format(Tmpl, [Port]);
		{error, Reason} ->
			Page = io_lib:format("<html><body><h2>File <i>~s</i> not found</h2></body></html>",
								 [FileName]
								)
	end,
	% Meth = Req:get(method),
	Req:ok([{"Content-Type", "text/html"}], [Page]).
	
%%
%% Connection manager. Manages connections and clients list.
%%
conn_manager() ->
	process_flag(trap_exit, true),
	conn_manager([]).

conn_manager(CurrentClients) ->
	receive 
		{new, Pid} ->
			Ref = erlang:monitor(process, Pid),
			% Ref = 0,
			io:fwrite("Process ~w added to client manager monitoring with ref ~w~n", [Pid, Ref]),
			NewClients = CurrentClients ++ [#client{pid=Pid, ref=Ref}],
			conn_manager(NewClients);
		{send_list, Pid} ->
			Pid ! CurrentClients,
			conn_manager(CurrentClients);
		{print_list} ->
			io:fwrite("Curr clients: ~w~n", [CurrentClients]),
			conn_manager(CurrentClients);
		{bcast, Msg} ->
			[Client#client.pid ! {send, Msg} || Client <- CurrentClients],
			conn_manager(CurrentClients);
		{quit} ->
			io:fwrite("Conn Manager quit ~n");
		{'DOWN', Ref, process, Pid, Reason} ->
			% working process goes down
			io:fwrite("Process ~w (Ref ~w) is down because of ~w~n", [Pid, Ref, Reason]),
			erlang:demonitor(Ref),
			NewClients = lists:filter(fun(X) -> X#client.ref =/= Ref end, CurrentClients),
			conn_manager(NewClients);
			% NewClients = [X || X <- CurrentClients, {_, FindRef} = X, FindRef /= Ref] ;
		Ignore ->
			io:fwrite("Conn Manager got unknown command: ~w~n", [Ignore]),
			conn_manager(CurrentClients)
	end.

% first call for websocket callback
handle_websocket_start(Ws) ->
	conn_mgr ! {new, self()},
	handle_websocket(Ws).

% callback on received websockets data
handle_websocket(Ws) ->
	receive
		{browser, Data} ->
			io:fwrite("Got Data: ~p~n", [Data]),
			Ws:send(["received '", Data, "'"]),
			conn_mgr ! {bcast, Data},
			_Z = xopt(Ws, Data),
			handle_websocket(Ws);
		{send, Data} ->
			io:fwrite("Got Data for send: ~p~n", [Data]),
			Ws:send([Data]),
			handle_websocket(Ws);			
		_Ignore ->
			Str = io_lib:format("Got msg _Ignore: ~p~n", [_Ignore]), 
			io:write(Str),
			Ws:send([Str]),
			handle_websocket(Ws)
	after 5000 ->
		Ws:send("pushing!"),
		handle_websocket(Ws)
	end.

% Xtra Options for incoming data
xopt(Ws, Data) ->
	case Data of
		"/U" ->
			Ws:send(["Got cmd /U. Russian text: ",  ?ub("")]);
		"/0" ->
			Ws:send(["Got cmd /0"]),
			(2+3) / (2 - 2);
		"/L" ->
			conn_mgr ! {send_list, self()};
		_ ->
			none
	end,
	% Ws:send(["test"]),
	ok.

xopt(Ws, Data, false) ->
	case Data of
		"/U" ->
			Ws:send(["Got cmd /U. Russian text: ",  unicode:characters_to_list("")]);
		"/0" ->
			Ws:send(["Got cmd /0"]),
			(2+3) / 2 
	end,
	ok.

	