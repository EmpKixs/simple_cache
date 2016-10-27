%%

-module(simple_cache_sup).
-behaviour(supervisor).
-export([init/1]).
-define(SERVER,?MODULE).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/0]).

start_link() ->
	supervisor:start_link({local,?SERVER}, ?MODULE,[]).


%% ====================================================================
%% Behavioural functions 
%% ====================================================================

%% init/1
init([]) ->
	ElementSup = {simple_cache_element_sup,{simple_cache_element_sup,start_link,[]},
				  permanent,2000,supervisor,[simple_cache_element]},
	EventManager = {simple_cache_event,{simple_cache_event,start_link,[]},
					permanent,2000,worker,[simple_cahce_event]},
	Children = [ElementSup,EventManager],
	RestartStrategy = {one_for_one,4,3600},
	{ok,{RestartStrategy,Children}}.


%% ====================================================================
%% Internal functions
%% ====================================================================


