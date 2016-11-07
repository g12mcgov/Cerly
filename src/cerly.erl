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
		<<"data">> := Data,
		<<"content_type">> := ContentType,
		<<"request">> := Request
		} = cerly_parser:parse_command(Command),
	Response = make_request(Request, Url, ContentType, Data, Tokens),
	{Status, {_, _, Body}} = Response,
	case cerly_parser:expects_json(ContentType) andalso Status =:= ok of
		true when Body =/= "ok" ->
			% If the content-type is JSON, serialize the response
			cerly_parser:serialize_json(Body);
		_ -> Body
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

make_request("GET", Url, ContentType, null, _Tokens) ->
	case ContentType of
		null -> void;
		_ -> io:format("Note: Recieved Content-Type ~p for GET request. This will be silently ignored.~n", [ContentType])
	end,
	httpc:request(get, 
		{
			Url,  		%% URL
			[]	  		%% HEADERS
		},
		[],				%% HTTP OPTIONS
		[]);			%% OPTIONS

% Null data body
make_request("POST", Url, ContentType, null, _Tokens) ->
	httpc:request(post, 
		{
			Url,		 %% URL
			[],			 %% HEADERS
			ContentType, %% CONTENT-TYPE
			[] 			 %% BODY
		}, 
		[],				 %% HTTP OPTIONS
		[]);			 %% OPTIONS

% Null Content-Type
make_request("POST", Url, null, Body, _Tokens) ->
	httpc:request(post, 
		{
			Url,		%% URL
			[],			%% HEADERS
			[],			%% CONTENT-TYPE
			Body 		%% BODY
		}, 
		[], 			%% HTTP OPTIONS
		[]); 			%% OPTIONS

% Null Content-Type
make_request("POST", Url, ContentType, Body, _Tokens) ->
	httpc:request(post, 
		{
			Url, 		 %% URL
			[], 		 %% HEADERS
			ContentType, %% CONTENT-TYPE
			Body 		 %% BODY
		}, 
		[],				 %% HTTP OPTIONS
		[]).			 %% OPTIONS
