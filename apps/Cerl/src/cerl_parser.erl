% @Author: gmcgovern1271
% @Date:   2016-10-31 14:58:09
% @Last Modified by:   gmcgovern1271
% @Last Modified time: 2016-10-31 15:28:42

-module(cerl_parser).
-export([parse/1]).
-include("macros.hrl").

%%%
%% Parse Curl response
%%
parse(CurlCommand) ->
	%% Valid Curl args
	OptSpecList = 
	[
		{'progress-bar' ,	$#, 	"progress-bar",		string,		"Displays progress bar"},
		{next 			,	$:, 	"next",				string,		"Tells Curl use a separate operation for the following URL and associated options"},
		{'http1.0'		, 	$O,		"http1.0",			string,		"Tells Curl to use HTTP v1.0 instead of v1.1"},
		{request		, 	$X,		"request",			string,		"Specifies request type (GET, POST, etc...)"}
	],
	Tokenized = getopt:parse(OptSpecList, CurlCommand),
	?trace(Tokenized).