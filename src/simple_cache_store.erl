-module(simple_cache_store).
-define(TABLE_ID,?MODULE).

-record(key_to_pid,{key,pid}).

%% ====================================================================
%% API functions
%% ====================================================================
-export([init/0,insert/2,look_up/1,delete/1]).

init() ->
	%%建ETS
	%%ets:new(?TABLE_ID, [public,named_table]),
	%%ok.
	%%Mnesia
	mnesia:start(),
	mnesia:create_table(key_to_pid, [{index,[pid]},
									 {attributes,record_info(fields,key_to_pid)}]).


insert(Key,Pid) ->
	%%ets:insert(?TABLE_ID, {Key,Pid}).
	mnesia:dirty_write(#key_to_pid{key=Key,pid=Pid}).
%% look_up(Key) -> %%ETS
%% 	case ets:lookup(?TABLE_ID, Key) of
%% 		[{Key,Pid}] ->
%% 				{ok,Pid};
%% 		[] ->
%% 			{error,not_found}
%% 	end.
look_up(Key) -> %%Mnesia
	case mnesia:dirty_read(key_to_pid, Key) of
		[{Key,Pid}] ->
			case is_pid_alive(Pid) of
				true ->
					%%无效的PID仍然还会存在Mnesia中，需要处理
					{ok,Pid};
				false ->
					{error,not_found}
			end;
		[] ->
			{error,not_found}
	end.
%% 检查PID进程是否还存活
is_pid_alive(Pid) when node(Pid) =:= node() ->
	is_process_alive(Pid);
is_pid_alive(Pid) ->
	lists:member(node(Pid), node()) andalso 
		(rpc:call(node(Pid), erlang, is_process_alive, [Pid]) =:= true ).


%% delete(Pid) -> %% ETS
%% 	ets:match_delete(?TABLE_ID, {'_',Pid}).
delete(Pid) ->
	case mnesia:dirty_index_read(key_to_pid, Pid, #key_to_pid.pid) of
		[#key_to_pid{} = Record] ->
			mnesia:dirty_delete(Record);
		_ ->
			ok
	end.


%% ====================================================================
%% Internal functions
%% ====================================================================


