%% Author: e
%% Created: 19.02.2010
%% Description: TODO: Add description to wschat
-module(wisechat).
%-behavior(application).

%%
%% Include files
%%

%%
%% Some defines for comfortable work
%%
-define(SERVERNAME, "WiseChat").

-define(VSN, "0.0.1").

-define(ub(X), unicode:characters_to_binary(X)).

%% for debug
%% Write debug:
-define(D(Msg), util:write_debug(Msg, ?LINE, ?FILE)).
% -define(D2(Fmt, Data), util:write_debug(Fmt, ?LINE, ?FILE)).
		
%%
%% Records
%%
-record(client,
	{
		pid,	%  Process handling websocket connection
		ref,	%  Ref to process for monitoring
		name=""	%  Client name
	}).

%%
%% Exported Functions
%%
-export([stop/0, start/1, start_work/1, stop_work/1]).

%% Write debug:
%% write_debug(Msg) ->
%% 	Now = erlang:now(),
%% 	{{Year, Month, Day}, {H, M, S}} = calendar:now_to_local_time(Now),
%% 	io:format("[~p-~p-~p ~p:~p:~p] ~p in ~p at ~p~n", 
%% 				[Year, Month, Day, H, M, S, Msg, ?FILE, ?LINE]).

%%
%% API Functions
%%
%start(StartType, StartArgs) ->       % {ok, Pid} | {ok, Pid, State}
%	Pid = start(8000),
%	{ok, Pid}.


%%
%% TODO: Add description of stop/function_arity
%%
start(Port) ->
	Conn_Manager = spawn(fun() -> conn_manager() end),
	% io:fwrite("Conn Manager Pid: ~w~n", [Conn_Manager]),
	% ?D1(io_lib:format("Conn Manager Pid: ~p~n", [Conn_Manager])),
	?D({"Conn Manager Pid: ~p", [Conn_Manager]}),
	% ?D("Conn Manager Pid"),
	register(conn_mgr, Conn_Manager),
	misultin:start_link([
		{port, Port}, 
		{loop, fun(Req) -> handle_http(Req, Port) end}, 
		{ws_loop, fun(Ws) -> handle_websocket_start(Ws) end}
	]),
	Conn_Manager.

%%
%% functions for running program
%%
start_work([Arg]) ->
	Port = list_to_integer(atom_to_list(Arg)),
	%io:format("Start_work started on port ~w ~n", [Port]),
	register(main_proc, self()),
	start(Port),
	continue_work().

continue_work() ->
	receive
		quit ->
			?D("Main_work proc got quit message"),
			ok;
		Else ->
			?D({"Main_work proc got unknown message: ~w", [Else]}),
			continue_work()
	end.


%%
%% TODO: Add description of stop/function_arity
%%
stop() ->
	io:format("Stopping..."),
	conn_mgr ! {quit},
	main_proc ! quit,
	misultin:stop(),
	ok.

%%
%% Stop the node from Joe's recipe.
%%
stop_work([Node]) ->
	io:format("Stop: ~p~n",[Node]),
	case net_adm:ping(Node) of
		pong ->
			ok;
		pang ->
			io:format("There is no node with this name~n")
	end,
	% rcp:call(Node, wisechat, stop, []),
	spawn(Node, wisechat, stop, []),
	rpc:cast(Node, init, stop, []),
	init:stop().

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

dispatch_http(_Meth='GET', UrlParts, Req, _Port) ->
	Path = "/" ++ string:join(UrlParts, "/"),
	?D({"Got path: ~p", [Path]}),
	case string:str(Path, "..") of
		0 ->
			% correct path
			{ok, Dir} = file:get_cwd(),
			FilePath = Dir ++ "/www/" ++ Path,
			% Check the file
			case filelib:is_file(FilePath) of
				true ->
					?D({"Path ~p ok, send file", [FilePath]}),
					Req:file(FilePath);
				false ->
					?D({"File ~p not found or is dir", [FilePath]}),
					dispatch_http_404(_Meth, UrlParts, Req, _Port)
			end;
		_Else ->
			% incorrect path
			?D("Path incorrect"),
			dispatch_http_404(_Meth, UrlParts, Req, _Port)
%% 			Req:respond(400, [{"Content-Type", "text/html"}], 
%% 				[
%% 					"File /", Path, " not found. <br /> \n\r",
%% 					"Method: " , atom_to_list(_Meth) 
%% 				])
	end;
	

dispatch_http(_Meth, _Path, Req, _Port) ->
	dispatch_http_404(_Meth, _Path, Req, _Port).
%% 	Req:respond(404, [{"Content-Type", "text/html"}], 
%% 		[
%% 			"File /", string:join(_Path, "/"), " not found. <br /> \n\r",
%% 			"Method: " , atom_to_list(_Meth) 
%% 		]).


%%
%% Send error
%%
dispatch_http_404(Meth, UrlParts, Req, Port) ->
	Req:respond(404, [{"Content-Type", "text/html"}], 
		[
			"File /", string:join(UrlParts, "/"), " not found. <br /> \n\r",
			"Method: " , atom_to_list(Meth),
			"Server: ", ?SERVERNAME, "/", ?VSN, " at port ", Port
		]).
	

%%
%% Index page for chat
%%

handle_http_index(Req, Port) ->	
	% output
	% io:fwrite("Sending index page...~n"),
	?D("Send index page"),
	FileName = "www/chat.html",
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
			?D({"Process ~w added to client manager monitoring with ref ~w", [Pid, Ref]}),
			NewClients = CurrentClients ++ [#client{pid=Pid, ref=Ref}],
			conn_manager(NewClients);
		{send_list, Pid} ->
			% Send clients list to user
			ClientsDisp = [Client#client.name || Client <- CurrentClients],
			Pid ! ClientsDisp,
			conn_manager(CurrentClients);
		{print_list} ->
			?D({"Curr clients: ~w", [CurrentClients]}),
			conn_manager(CurrentClients);
		{bcast, Msg} ->
			[Client#client.pid ! {send, Msg} || Client <- CurrentClients],
			conn_manager(CurrentClients);
		{quit} ->
			?D("Conn Manager quit.");
		{'DOWN', Ref, process, Pid, Reason} ->
			% working process goes down
			?D({"Process ~w (Ref ~w) is down because of ~w", [Pid, Ref, Reason]}),
			erlang:demonitor(Ref),
			NewClients = lists:filter(fun(X) -> X#client.ref =/= Ref end, CurrentClients),
			conn_manager(NewClients);
			% NewClients = [X || X <- CurrentClients, {_, FindRef} = X, FindRef /= Ref] ;
		Ignore ->
			?D({"Conn Manager got unknown command: ~w", [Ignore]}),
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
			?D({"Got Data: ~p", [Data]}),
			Ws:send(["received '", Data, "'"]),
			conn_mgr ! {bcast, Data},
			_Z = xopt(Ws, Data),
			handle_websocket(Ws);
		{send, Data} ->
			?D({"Got Data for send: ~p", [Data]}),
			Ws:send([Data]),
			handle_websocket(Ws);			
		_Ignore ->
			Str = io_lib:format("Got msg _Ignore: ~p~n", [_Ignore]), 
			% io:write(Str),
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

	