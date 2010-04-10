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

-define(ul(X), unicode:characters_to_list(X)).

-define(ARCHIVE_LAST, 512).

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
		name="",	%  Client name
		color=""  % Client color
	}).

-record(session,
	{
		name="",	% Client name in session
		auth=false,	% Auth state     | true
		color=""  	% Client color
	}).

% client manager state
-record(cm_state,
	{
		clients = [],	% Clients list
		msg_buf = [] 	% messages buffer for later clients
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
	% random_p:start(),
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
	% random:seed(),
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
	Path = Req:resource([urldecode]),
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
	?D({"Dispatch 404, Meth ~p; UrlParts ~p; Req ~p; Port ~p", [Meth, UrlParts, Req, Port]}),
	Req:respond(404, [{"Content-Type", "text/html"}],
		[
			"File /", string:join(UrlParts, "/"), " not found. <br /> \n\r",
			"Method: " , atom_to_list(Meth), "\n\r",
			"Server: ", ?SERVERNAME, "/", ?VSN, " at port ", erlang:integer_to_list(Port)
		])
	.


%%
%% Index page for chat
%%

handle_http_index(Req, Port) ->
	% output
	% io:fwrite("Sending index page...~n"),
	?D("Send index page"),
	FileName = "www/index.html",
	FileRes = file:read_file(FileName),
	case FileRes of
		{ok, Tmpl} ->
			% Page = io_lib:format(Tmpl, [Port]);
			Page = Tmpl;
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
	CM_State = #cm_state{},
	conn_manager(CM_State).

conn_manager(CurrentState) ->
	?D({"Conn manager loop. State: ~p", [CurrentState]}),
	CurrentClients = CurrentState#cm_state.clients,
	receive
		{new, Pid} ->
			Ref = erlang:monitor(process, Pid),
			% Ref = 0,
			?D({"Process ~w added to client manager monitoring with ref ~w", [Pid, Ref]}),
			% NewClients = CurrentClients ++ [#client{pid=Pid, ref=Ref}],
			NewClient = #client{pid=Pid, ref=Ref},
			% NewClients = lists:keymerge(4, [NewClient], CurrentState#cm_state.clients),
			NewClients = [NewClient | CurrentClients],
			?D({"New Clients: ~p", [NewClients]}),
			clients_send_list(all, NewClients),
			NewState = CurrentState#cm_state{clients=NewClients},
			conn_manager(NewState);
		{set_opts, Pid, Session} ->
			% Set client options from session data
			?D({"Set clients options '~p' for Pid ~p", [Session, Pid]}),
			Res  = lists:keytake(Pid, 2, CurrentClients),
			case Res of
				{value, OldClient, RestClients} ->
					% client is found
					?D({"Client: ~p. Clients list after deletion: ~p", [OldClient, RestClients]}),
					% {value, OldClient, RestClients}
					NewClient = OldClient#client{name=Session#session.name, color=Session#session.color},
					% NewClients = lists:keymerge(4, [NewClient], RestClients),    %%%   TROUBLE
					NewClients = [NewClient | RestClients],
					NewState = CurrentState#cm_state{clients=NewClients};
				false ->
					% not found
					?D({"Cannot find client! ", []}),
					NewClients = CurrentClients,
					NewState = CurrentState  % #cm_state{clients=Clients},
					% NewClients = CurrentClients
			end,
			clients_send_list(all, NewClients),
			conn_manager(NewState);
		{send_list, Pid} ->
			?D("Send clients list to user"),
			clients_send_list(Pid, CurrentClients),
			% Send clients list to user
			% ClientsDisp = [?ub(Client#client.name) || Client <- CurrentClients],
			% Msg = [{"users", ClientsDisp}],
			% MsgJson = rfc4627:encode({obj, Msg}),
			% Pid ! {send, MsgJson},
			conn_manager(CurrentState);
		{print_list} ->
			?D({"Curr clients: ~w", [CurrentClients]}),
			conn_manager(CurrentState);
		{bcast, Msg} ->
			?D({"Message to broadcast: ~p", [Msg]}),
			Messages = CurrentState#cm_state.msg_buf,
			NewMessages = add_to_history(Messages, Msg),
			?D({"Full messages archive is: ~p", [NewMessages]}),
			[Client#client.pid ! {send, Msg} || Client <- CurrentClients],
			NewState = CurrentState#cm_state{msg_buf=NewMessages},
			conn_manager(NewState);
		{archive, Pid} ->
			?D({"Sending archive to Pid: ~p", [Pid]}),
			Messages = CurrentState#cm_state.msg_buf,
			[Pid ! {send, Msg} || Msg <- Messages],
			conn_manager(CurrentState);
		{quit} ->
			?D("Conn Manager quit.");
		{'DOWN', Ref, process, Pid, Reason} ->
			% working process goes down
			?D({"Process ~w (Ref ~w) is down because of ~w", [Pid, Ref, Reason]}),
			erlang:demonitor(Ref),
			NewClients = lists:filter(fun(X) -> X#client.ref =/= Ref end, CurrentClients),
			clients_send_list(all, NewClients),
			NewState = CurrentState#cm_state{clients=NewClients},
			conn_manager(NewState);
			% NewClients = [X || X <- CurrentClients, {_, FindRef} = X, FindRef /= Ref] ;
		Ignore ->
			?D({"Conn Manager got unknown command: ~w", [Ignore]}),
			conn_manager(CurrentState)
	end.


% Send all clients to all users
% clients_send_list(Pids, CurrentClients) when is_list(Pids) ->
% 	?D({"Sendign all clients list to all clients: ~p ", [CurrentClients]})
% 	[client_send_list(Client#client.pid, CurrentClients) || Client <- CurrentClients];

% Send all clients list to the requested Pid
clients_send_list(Pid, CurrentClients) ->
	ClientsDisp = [?ub(io_lib:format("<span style='color:~ts'>~ts</span>", [Client#client.color, Client#client.name]))
					|| Client <- CurrentClients, Client#client.name /= ""],
	% ClientsDisp = [ {obj, [{"name", ?ub(C#client.name)}, {"color", ?ub(C#client.color)}]}
	% 				|| C <- CurrentClients, C#client.name /= "" ],
	% Anons = lists:foldl(fun(X, Sum) -> )
	?D({"ClientsDisp is: ~p", [ClientsDisp]}),
	Anons = length([ Client || Client <- CurrentClients, Client#client.name == ""]),
	Msg = [{"users", ClientsDisp}, {"anons", Anons}],
	?D({"Whole clients display is: ~p", [Msg]}),
	MsgJson = rfc4627:encode({obj, Msg}),
	?D({"Whole clients json is:  ~s", [MsgJson]}),
	case Pid of
		all ->
			% send to all users
			[Client#client.pid ! {send, MsgJson} || Client <- CurrentClients];
		XPid when is_pid(XPid) ->
			XPid ! {send, MsgJson};
		_ ->
			% error
			?D("Don't know to whom send client list")
	end.


% Append message to list and trim it at 1000
add_to_history(List, Message) ->
	NewList = List ++ [Message],
	?D({"New history list is: ~p", [NewList]}),
	Len = length(NewList),
	?D({"New history list length: ~p", [Len]}),
	Num = Len - ?ARCHIVE_LAST,
	TakeNum = case Num of
				_ when Num > 0 ->
					Num;
				_ ->
					0 % ?ARCHIVE_LAST
			end,
	?D({"Nthtail from: ~p", [TakeNum]}),
	lists:nthtail(TakeNum, NewList).

% Auth function
check_credentials(Auth) ->
	?D({"Auth data is ~p", [Auth]}),
	NameBin = proplists:get_value("name", Auth),
	?D({"NameBin is ~p", [NameBin]}),
	Name = ?ul(NameBin),
	?D({"User name is ~p", [Name]}),
	{ok, Name}.

% first call for websocket callback
handle_websocket_start(Ws) ->
	{A1, A2, A3} = now(),
	random:seed(A1, A2, A3),
	?D({"Ws process started with Pid ~p", [self()]}),
	conn_mgr ! {new, self()},
	Session = #session{name="", auth=false},
	handle_websocket(Ws, Session).

% first auth user
% handle_websocket_auth(Ws, Session) ->
% 	?D("Auth new user"),
% 	receive
% 		{browser, Data} ->
% 			?D({"Auth got data: ~p", [Data]}),
% 			% BinData = ?ub(Data),
% 			% ?D({"Bin auth data: ~p", [BinData]}),
% 			{ok, {obj, Dec}, _Rest} = rfc4627:decode(Data),
% 			?D({"Decoded auth data: ~p", [Dec]}),
% 			case check_credentials(Dec) of
% 				{ok, Name} ->
% 					% Auth ok
% 					?D("Auth ok"),
% 					Msg = [{"auth_ok", 1}],
% 					Ws:send(rfc4627:encode({obj, Msg})),
% 					% Add to the list
% 					conn_mgr ! {new, self(), Name},
% 					% Get users list
% 					conn_mgr ! {send_list, self()},
% 					% продолжаем в основном режиме
% 					handle_websocket(Ws);
% 				_Else ->
% 					% Auth error
% 					?D("Auth error"),
% 					handle_websocket_auth(Ws)
% 			end;
% 		_Ignore ->
% 			?D({"Handle auth got unknown message: ~p", [_Ignore]})
% 	end.

% callback on received websockets data
handle_websocket(Ws, Session) ->
	receive
		{browser, Data} ->
			?D({"Got Data: ~p", [Data]}),
			% Ws:send(["received '", Data, "'"]),
			{ok, {obj, Dec}, _Rest} = rfc4627:decode(Data),
			?D({"Decoded data: ~p", [Dec]}),
			SrcMsg = proplists:get_value("msg", Dec),
			?D({"SrcMsg is ~p", [SrcMsg]}),
			% Msg = util:strip_tags(SrcMsg),
			% ?D({"Msg is ~p", [Msg]}),
			case SrcMsg of
				undefined ->
					% это не сообщение
					Cmd = proplists:get_value("cmd", Dec),
					?D({"Cmd is ~p", [Cmd]}),
					case Cmd of
						<<"auth">> ->
							% Do auth
							{ok, Name} = check_credentials(Dec),
							StrippedName = util:strip_tags(Name),
							R = random:uniform(127),
							G = random:uniform(127),
							B = 250 - R - G,
							Color = io_lib:format("rgb(~w,~w,~w)", [R, G, B]),
							NewSession = Session#session{name=StrippedName, auth=true, color=Color},
							conn_mgr ! {set_opts, self(), NewSession},
							Reply = [{"auth_ok", 1}],
							Ws:send(rfc4627:encode({obj, Reply})),
							?D({"New Session is ~p", [NewSession]}),
							handle_websocket(Ws, NewSession);
						<<"send_list">> ->
							% Get users list
							conn_mgr ! {send_list, self()},
							handle_websocket(Ws, Session);
						<<"archive">> ->
							?D({"Send archive to user", []}),
							conn_mgr ! {archive, self()},
							handle_websocket(Ws, Session);
						_Else ->
							% Unknown command
							?D({"Got unknown command: ~p", [Cmd]}),
							handle_websocket(Ws, Session)
					end;
				_ ->
					% сообщение - проверить права пользователя
					Authed = Session#session.auth,
					?D({"Auth state ~p", [Authed]}),
					case Authed of
						true ->
							% Client authentificated
							StrippedMsg = util:strip_tags(?ul(SrcMsg)),
							Reply = [{"msg", ?ub(StrippedMsg)}, {"name", ?ub(Session#session.name)},
									{"color", ?ub(Session#session.color)}],
							Json = rfc4627:encode({obj, Reply}),
							?D({"Json to send: ~p", [Json]}),
							conn_mgr ! {bcast, Json};
						false ->
							% unauthentificated
							?D({"Session ~p is not authentificated", [Session]}),
							?D("Client is not authentificated - message ignored"),
							Ws:send(["Unathentificated"])
					end,
					handle_websocket(Ws, Session)
			end;
			% _Z = xopt(Ws, Msg),
			% handle_websocket(Ws, Session);
		{send, Data} ->
			?D({"Got Data for send: ~p", [Data]}),
			Ws:send([Data]),
			handle_websocket(Ws, Session);
		_Ignore ->
			Str = io_lib:format("Got msg _Ignore: ~p~n", [_Ignore]),
			% io:write(Str),
			Ws:send([Str]),
			handle_websocket(Ws, Session)
	after 5000 ->
		% Ws:send("pushing!"),
		handle_websocket(Ws, Session)
	end.

% Xtra Options for incoming data
xopt(Ws, Data) ->
	case Data of
		"/U" ->
			Ws:send(["Got cmd /U. Russian text: ",  "Русский текст", ?ub("Еще по-русски.")]);
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

