%%file_comment

-module(simple_cache_event_logger).
-behaviour(gen_event).
-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([add_handler/0,delete_handler/0]).

add_handler() ->
	simple_cache_event:add_handler(?MODULE, []).

delete_handler() ->
	simple_cache_event:delete_handler(?MODULE, []).



%% ====================================================================
%% Behavioural functions 
%% ====================================================================
-record(state, {}).

%% init/1

init([]) ->
    {ok, #state{}}.


%% handle_event/2
handle_event({create,{Key,Value}},State) ->
	error_logger:info_msg("create{~w,~w}~n", [Key,Value]),
	{ok,State};
handle_event({lookup,Key},State) ->
	error_logger:info_msg("lookup(~w)~n", [Key]),
	{ok,State};
handle_event({delete,Key},State) ->
	error_logger:info_msg("delete(~w)~n",[Key]),
	{ok,State};
handle_event({replace,{Key,Value}},State) ->
	error_logger:info_msg("replace(~w,~w)~n",[Key,Value]),
	{ok,State};
handle_event(_Event, State) ->
    {ok, State}.


%% handle_call/2
handle_call(_Request, State) ->
    Reply = ok,
    {ok, Reply, State}.


%% handle_info/2
handle_info(_Info, State) ->
    {ok, State}.


%% terminate/2
terminate(_Arg, _State) ->
    ok.


%% code_change/3
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% ====================================================================
%% Internal functions
%% ====================================================================


