%%%-------------------------------------------------------------------
%%% @author Grant McGovern (gmcgovern1271)
%%% @doc
%%% File containing project-wide macros definitions
%%% @end
%%% Created : 16. Jun 2016 13:26
%%%-------------------------------------------------------------------

%% Trace
-define(trace(X), io:format(<<"[~p:~p] ~p~n">>,[?MODULE,?LINE,X])).
