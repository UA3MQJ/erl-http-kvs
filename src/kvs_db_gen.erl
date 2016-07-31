-module(kvs_db_gen).
-behaviour(gen_server).

%%API
-export([start_link/0]).
-export([select/1, insert/3, update/3, delete/1, exist/1]).

%%GEN_SERVER callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

select(Key) -> 
    gen_server:call(?SERVER, {select, Key}).

insert(Key, Value, Ttl) ->
    gen_server:call(?SERVER, {insert, Key, Value, Ttl}).

update(Key, Value, Ttl) ->
    gen_server:call(?SERVER, {update, Key, Value, Ttl}).

delete(Key) ->
    gen_server:call(?SERVER, {delete, Key}).

exist(Key) ->
  gen_server:call(?SERVER, {exist, Key}).

init([]) ->
  dets:open_file(base, [{type, set}]). % return {ok, Ref}  % Ref is State

handle_call({select, Key}, _From, State) ->
  {reply, get_value(State, Key), State};

handle_call({insert, Key, Value, Ttl}, _From, State) ->
  {reply, dets:insert(State, {Key, {Value, Ttl, erlang:timestamp()}}), State}; %% see timer:now_diff/2.

handle_call({update, Key, Value, Ttl}, _From, State) ->
  {reply, dets:insert(State, {Key, {Value, Ttl, erlang:timestamp()}}), State};

handle_call({delete, Key}, _From, State) ->
  {reply, dets:delete(State, Key), State};

handle_call({exist, Key}, _From, State) ->
  {reply, get_value(State, Key)/=null, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    dets:close(State),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

get_value(State, Key) ->
  case dets:lookup(State, Key) of
    [] -> null;
    [{Key, {Value, Ttl, CreatedTime}}] ->
     RealTTL = timer:now_diff(erlang:timestamp(), CreatedTime),
     case ((RealTTL/1000000) < Ttl) of
       true ->
          Value;
       _ ->
         dets:delete(State, Key), %% remove when exist
         null
     end;
    _ -> null
  end.