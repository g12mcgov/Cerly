%%%-------------------------------------------------------------------
%% @doc cerl public API
%% @end
%%%-------------------------------------------------------------------

-module(cerl_app).
-include("macros.hrl").

%% Application callbacks
-export([cerl/1]).

%%====================================================================
%% API
%%====================================================================
cerl(Command) when is_binary(Command) ->
	{error, "Sorry, binary strings are not yet supported."};
cerl(Command) ->
	cerl_parser:parse(Command).
	%Out = os:cmd(Command),
	%?trace(Out).

%%====================================================================
%% Internal functions
%%====================================================================
