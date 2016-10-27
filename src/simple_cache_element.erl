-module(simple_cache_element).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([create/1,create/2,delete/1,fetch/1,replace/2,start_link/2]).
-define(DEFAULT_LEASE_TIME,60*60*24).


start_link(Value,LeaseTime) ->
	gen_server:start_link(?MODULE, [Value,LeaseTime], []).

create(Value) ->
	create(Value, ?DEFAULT_LEASE_TIME).

create(Value,LeaseTime) ->
	simple_cache_element_sup:start_child(Value, LeaseTime).

fetch(Pid) ->
	gen_server:call(Pid	,fetch).

replace(Pid,Value) ->
	gen_server:cast(Pid, {replace,Value}).

delete(Pid) ->
	gen_server:cast(Pid, delete).


%% ====================================================================
%% Behavioural functions 
%% ====================================================================
-record(state, {value,lease_time,start_time}).

%% init/1
init([Value,LeaseTime]) ->
    Now = calendar:local_time(),
	StartTime = calendar:datetime_to_gregorian_seconds(Now),
	{ok,#state{value=Value,lease_time=LeaseTime,start_time=StartTime},
	 	time_left(LeaseTime,StartTime)}.


time_left(_LeaseTime,infinity) ->
	infinity;
time_left(LeaseTime,StartTime) ->
	Now = calendar:local_time(),
	CurrentTime = calendar:datetime_to_gregorian_seconds(Now),
	TimeElapased = CurrentTime - StartTime,
	case LeaseTime - TimeElapased of
		Time when Time =< 0 ->
			0;
		Time ->
			Time*1000
	end.


%% handle_call/3
handle_call(fetch, _From, State) ->
    #state{value = Value,lease_time=LeaseTime,start_time=StartTime} = State,
	TimeLeft = time_left(LeaseTime,StartTime),
	{reply, {ok,Value}, State,TimeLeft}.


%% handle_cast/2
handle_cast({replace,Value}, State) ->
	#state{lease_time=LeaseTime,start_time=StartTime} = State,
	TimeLeft = time_left(LeaseTime, StartTime),
	{noreply, State#state{value=Value},TimeLeft};
handle_cast(delete,State)->
	{stop,normal,State}.


%% handle_info/2
handle_info(timeout, State) ->
    {stop,normal, State}.


%% terminate/2
terminate(_Reason, _State) ->
    simple_cache_store:delete(self()),
	ok.


%% code_change/3
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% ====================================================================
%% Internal functions
%% ====================================================================


