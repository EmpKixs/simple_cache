-module(simple_cache).

%% ====================================================================
%% API functions
%% ====================================================================
-export([insert/2,look_up/1,delete/1]).

insert(Key,Value) ->
	case simple_cache_store:look_up(Key) of
		{ok,Pid} ->
			simple_cache_element:replace(Pid, Value),
			simple_cache_event:replace(Key, Value);
		{error,_} ->
			{ok,Pid} = simple_cache_element:create(Value),
			simple_cache_store:insert(Key, Pid),
			simple_cache_event:create(Key, Value)
	end.

look_up(Key) ->
	case simple_cache_store:look_up(Key) of
		{ok,Pid} ->
			{ok,Value} = simple_cache_element:fetch(Pid),
			simple_cache_event:lookup(Key),
			{ok,Value};
		Reason ->
			Reason
	end.


delete(Key) ->
	case simple_cache_store:look_up(Key) of
		{ok,Pid} ->
			simple_cache_element:delete(Pid),
			simple_cache_event:delete(Key);
		{error,_Reson} ->
			ok
	end.

%% ====================================================================
%% Internal functions
%% ====================================================================


