%%%-------------------------------------------------------------------
%%% @author Grant McGovern (gmcgovern1271)
%%% @doc
%%% File containing project-wide macros definitions
%%% @end
%%% Created : 6. November 2016 13:26
%%%-------------------------------------------------------------------

%% Trace
-define(trace(X), io:format(<<"[~p:~p] ~p~n">>, [?MODULE, ?LINE, X])).

%% Ternary If
-define(iff(Expression, If, Else), 
	case Expression of 
		false -> Else; 
		_ -> If 
	end).
-define(iff(Expression, If), ?iff(Expression, If, void)).
