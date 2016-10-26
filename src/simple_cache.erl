-module(simple_cache).

%% ====================================================================
%% API functions
%% ====================================================================
-export([insert/2,look_up/1,delete/1]).

insert(Key,Value) ->
	case simple_cache_store:look_up(Key) of
		{ok,Pid} ->
			simple_cache_element:replace(Pid, Value);
		{error,_} ->
			{ok,Pid} = simple_cache_element:create(Value),
			simple_cache_store:insert(Key, Pid)
	end.

look_up(Key) ->
	try
		{ok,Pid} = simple_cache_store:look_up(Key),
		{ok,Value} = simple_cache_element:fetch(Pid),
		{ok,Value}
	catch
		_Class:_Exception ->
			{error,not_found}
	end.

delete(Key) ->
	case simple_cache_store:look_up(Key) of
		{ok,Pid} ->
			simple_cache_element:delete(Pid);
		{error,_Reson} ->
			ok
	end.

%% ====================================================================
%% Internal functions
%% ====================================================================


