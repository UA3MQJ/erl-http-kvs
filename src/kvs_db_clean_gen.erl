-module(kvs_db_clean_gen).
-behaviour(gen_server).

-define(INTERVAL, 60000). % One minute

-export([start_link/0]).
-export([init/1, handle_info/2, handle_call/3, handle_cast/2]).
-export([code_change/3, terminate/2]).

-define(SERVER, ?MODULE).

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
  erlang:send_after(?INTERVAL, self(), trigger),
  {ok, []}.

handle_info(trigger, State) ->
  kvs_db_gen:clean(),
  erlang:send_after(?INTERVAL, self(), trigger),
  {noreply, State}.

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

terminate(_Reason, _State) -> ok.
