%%%-------------------------------------------------------------------
%% @doc kvs top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(kvs_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    {ok, { {one_for_one, 5, 10}, [ ?CHILD(kvs_db_gen,worker),
                                    ?CHILD(kvs_http_gen,worker) ]} }.

%%====================================================================
%% Internal functions
%%====================================================================
