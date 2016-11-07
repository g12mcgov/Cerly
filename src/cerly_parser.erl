% @Author: gmcgovern1271
% @Date:   2016-10-31 14:58:09
% @Last Modified by:   Grant McGovern
% @Last Modified time: 2016-11-06 23:06:32

-module(cerly_parser).
-export([parse_command/1, 
		expects_json/1, 
		serialize_json/1,
		extract_content_type/1]).
-include("macros.hrl").

%%%
%% Currently the only curl args supported now.
%%%
supported_args() ->
	[
		{'progress-bar' 		, $#, 			"progress-bar",		string,		"Displays progress bar"},
		{next 					, $:, 			"next",				string,		"Tells Curl use a separate operation for the following URL and associated options"},
		{'http1.0'				, $O,			"http1.0",			string,		"Tells Curl to use HTTP v1.0 instead of v1.1"},
		{'request'				, $X,			"request",			string,		"Specifies request type (GET, POST, etc...)"},
		{header	 				, $H, 			"headers",			string,		"Headers"},
		{cookie					, $b,			"cookie",			string,		"Cookies"},
		{data					, $d,			"data",				string, 	"Data body"},
		{'user-agent'			, $A,			"user-agent",		string,		"(HTTP) Specify the User-Agent string to send to the HTTP server."},
		{'connect-timeout'		, undefined, 	"connect-timeout",	string,		"Maximum time in seconds that you allow curl's connection to take."},
		{form 					, $F,			"form",				string, 	"Lets curl emulate a filled-in form in which a user has pressed submit."},
		{include				, $i,			"include",			string, 	"(HTTP) Include the HTTP-header in the output."}
	].

%%%
%% Parse Curl response
%%
parse_command(CurlCommand) ->
	OptSpecList = supported_args(),
	%% Valid Curl args
	case begins_with_curl(CurlCommand) of
		true ->
			%% Check that infact the first command is "curl"
			Tokenized = getopt:parse(OptSpecList, CurlCommand),
			process_tokens(Tokenized);
		_ -> invalid_command("Command must begin with 'curl'"),
			[]
	end.

%%%
%% Check that the first 4 letters of the command are "curl"
%%%
begins_with_curl(Command) ->
	case string:substr(Command, 1, 4) of
		"curl" -> true;
		_ -> false
	end.

%%%
%% Validates response from getopt
%%%
process_tokens({ok, {Tokens}}) -> Tokens;
process_tokens({ok, {Tokens, Ignored}}) ->
	% In this case, we had some unmatched args from the above spec
	Transformed = transform_tokens(Tokens),
	Processed = process_ignored(Ignored),
	#{<<"tokens">> => Tokens,
	  <<"tokens_transformed">> => Transformed,
	  <<"url">> => hd(Processed),
	  <<"data">> => extract_data_body(Tokens),
	  <<"request">> => extract_request_type(Tokens),
	  <<"content_type">> => extract_content_type(Tokens)
	}.

%%
%% Converts a token list to map
%%
transform_tokens(Tokens) ->
	Formatted = lists:map(
		fun(X) ->
			{Key, _} = X, 
			#{Key => X}
		end, 
	Tokens),
	Formatted.

%%
%% Lookup in list and return corresponding value for a key
%%
lookup_in_list(Key, Tokens) ->
	case lists:keyfind(Key, 1, Tokens) of
        {Key, Result} -> Result;
        false -> null
    end.

%%
%% Extracts the raw Content-Type.
%%
%% So if we pass "Content-Type: application/json", it extracts
%% just the "application/json" part. This may seem counterintuitive
%% since we then build this tuple again later in the HTTP request,
%% but this allows us to later check quickly if the curl request is
%% expecting a JSON response.
extract_content_type(Tokens) ->
	case lookup_in_list(header, Tokens) of
		null -> null;
		ContentType ->
			Split = string:tokens(ContentType, ":"),
			strip_whitespace(string:join(tl(Split), "")) % Get last item (i.e. application/json)
	end.

%%
%% Extracts request type
%%
extract_request_type(Tokens) ->
	case lookup_in_list(request, Tokens) of
		null -> "GET"; % Default to GET if no specified type
		RequestType -> RequestType
	end.

%%
%% Extracts data body
%%
extract_data_body(Tokens) ->
	case lookup_in_list(data, Tokens) of
		null -> null;
		Data -> Data
	end.

%%%
%% Some Curl arguments don't take the standard Unix --[arg] [value] format, so
%% handle anything that isn't picked up in the passed command (such as the URL) 
%%%
process_ignored(Ignored) -> process_ignored(Ignored, []).
process_ignored([], Acc) -> Acc;
process_ignored([H | [] ], Acc) ->
	process_ignored([], [H | Acc]);
process_ignored([H | T], Acc) ->
	case H of
		"Content-Type:" ->
			process_ignored(tl(T), [#{<<"Content-Type">> => hd(T)} | Acc]);
		"curl" -> 
			process_ignored(T, Acc); % If we come across "curl", skip it
		_ -> process_ignored(T, [H | Acc])
	end.

%%%
%% Check if the curl command is to read JSON
%%%
expects_json("application/json") -> true;
expects_json(_) -> false.

%%%
%% Print rather than log
%%%
invalid_command(Error) ->
	io:format("Invalid command: ", Error).

%%%
%% Serializes a JSON Body response using Jiffy
%%%
serialize_json(Body) ->
	try jiffy:decode(Body) of
		Output -> {ok, Output}
	catch
		exit:Exit -> {error, Exit}
	end.

%%%
%% Clean whitespace from string
%%%
strip_whitespace(String) ->
	re:replace(String, "(^\\s+)|(\\s+$)", "", [global,{return,list}]).

