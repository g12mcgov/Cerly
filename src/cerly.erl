%%%-------------------------------------------------------------------
%% @doc cerly public API
%% @end
%%%-------------------------------------------------------------------

-module(cerly).
-behavior(application).
-include("macros.hrl").

%% Application callbacks
-export([start/2, stop/1]).
-export([cerly/1]).
%% Application callbacks
-compile([export_all]).

%%====================================================================
%% Rebar callbacks
%%====================================================================
start(_StartType, _StartArgs) ->
	application:start(inets),
	application:start(ssl),
    myapp_sup:start_link().

stop(_State) -> ok.

%%====================================================================
%% API
%%====================================================================
cerly(Command) when is_binary(Command) ->
	{error, "Sorry, binary strings are not yet supported."};
cerly(Command) ->
	#{<<"tokens">> := Tokens, 
		<<"url">> := Url,
		<<"content_type">> := ContentType,
		<<"request">> := Request
		} = cerly_parser:parse_command(Command),
	Response = make_request(Request, Url, ContentType, Tokens),
	case cerly_parser:expects_json(ContentType) of
		true ->
			% If the content-type is JSON, serialize the response
			cerly_parser:serialize_json(Response);
		_ -> Response
	end.

%%====================================================================
%% Internal functions
%%====================================================================

%%
%% Make HTTP requests
%%
make_request(Url, ContentType, _Tokens) when is_list(_Tokens) ->
	%% Since Curl defaults to 'GET', so will we
	httpc:request(get, {Url, [{"Content-Type", ContentType}]}, [], []).
make_request("GET", Url, ContentType, _Tokens) ->
	httpc:request(get, {Url, [{"Content-Type", ContentType}]}, [], []);
% Null Content-Type
make_request("POST", Url, null, Body, _Tokens) ->
	httpc:request(post, 
		{Url, []}, Body, []).
make_request("POST", Url, ContentType, _Tokens) ->
	httpc:request(post, 
		{Url, [{"Content-Type", ContentType}]}, [], []).
