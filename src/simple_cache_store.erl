-module(simple_cache_store).
-define(TABLE_ID,?MODULE).

%% ====================================================================
%% API functions
%% ====================================================================
-export([init/0,insert/2,look_up/1,delete/1]).

init() ->
	ets:new(?TABLE_ID, [public,named_table]),
	ok.

insert(Key,Pid) ->
	ets:insert(?TABLE_ID, {Pid,Key}).

look_up(Key) ->
	case ets:lookup(?TABLE_ID, Key) of
		[{Key,Pid}] ->
				{ok,Pid};
		[] ->
			{error,not_found}
	end.

delete(Pid) ->
	ets:match_delete(?TABLE_ID, {'_',Pid}).

%% ====================================================================
%% Internal functions
%% ====================================================================


