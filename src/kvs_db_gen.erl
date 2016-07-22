-module(kvs_db_gen).
-behaviour(gen_server).

%% databasr file name
-define(DBASE_FILE, "base.dat").

%%API
-export([start_link/0]).
-export([select/1, insert/2, update/2, delete/1]).

%%GEN_SERVER callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

select(Key) -> 
    gen_server:call(?SERVER, {select, Key}).

insert(Key, Value) ->
    gen_server:call(?SERVER, {insert, Key, Value}).

update(Key, Value) ->
    gen_server:call(?SERVER, {update, Key, Value}).

delete(Key) ->
    gen_server:call(?SERVER, {delete, Key}).

init([]) ->
    %File_name = code:priv_dir(us)++"/"++?DBASE_FILE,
    FileName = "/home/sea/erl-http-kvs/priv/"++?DBASE_FILE,
    {ok, Ref} = dets:open_file(FileName),
    State = Ref,
    {ok, State}.

handle_call({select, Key}, _From, State) ->
  Response = case dets:lookup(State, Key) of
               [] -> null;
               [{Key, Value}] -> Value
             end,
  {reply, Response, State};

handle_call({insert, Key, Value}, _From, State) ->
  Response = dets:insert(State, {Key, Value}),
  {reply, Response, State};

handle_call({update, Key, Value}, _From, State) ->
  Response = dets:insert(State, {Key, Value}),
  {reply, Response, State};

handle_call({delete, Key}, _From, State) ->
  Response = dets:delete(State, Key),
  {reply, Response, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    dets:close(State),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
