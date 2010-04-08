%% Author: e
%% Created: 31.03.2010
%% Description: TODO: Add description to util
-module(util).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([write_debug/3, strip_tags/1, escape_char/1]).

%%
%% API Functions
%%
%% Write debug:
write_debug(Tuple, Line, File) when is_tuple(Tuple) ->
	{Format, Data} = Tuple,
	% io:format("Format: ~p Data ~p", [Format, Data]),
	Msg = io_lib:format(Format, Data),
	write_debug(Msg, Line, File);

write_debug(Msg, Line, File)->
	Now = erlang:now(),
	{{Year, Month, Day}, {H, M, S}} = calendar:now_to_local_time(Now),
	io:format("[~4..0b-~2..0b-~2..0b ~2..0b:~2..0b:~2..0b] ~s. File ~p at line ~p~n",
				[Year, Month, Day, H, M, S, Msg, File, Line]).



%%
%% Local Functions
%%

% Delete < and > symbols from input
% strip_tags(S) ->
% 	lists:filter(fun (X) -> X /= $< andalso X /= $> end, S).

strip_tags(S) ->
	lists:map(fun escape_char/1, S).


escape_char(C) ->
	case C of
		$< -> "&lt;";
		$> -> "&gt;";
		$& -> "&amp;";
		10 -> 32;
		13 -> 32;
		_  -> C
	end.
