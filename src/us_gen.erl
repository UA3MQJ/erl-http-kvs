-module(us_gen).
-behaviour(gen_server).

%%API
-export([start_link/0, get_users/0, get_error/0]).
-export([login/2, register/2]).

%%GEN_SERVER callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
%% user list file name
-define(USER_LIST_FILE, "userlist3.json").

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

get_users() -> 
    gen_server:call(?SERVER, {get_users}).

get_error() ->
    gen_server:call(?SERVER, {get_error}).

login(User,Pass) ->
    gen_server:call(?SERVER, {login,User,Pass}).

register(User,Pass) ->
    gen_server:call(?SERVER, {register,User,Pass}).

init([]) ->
    UserList = load_user_list(),
    State = UserList,
    {ok, State}.

handle_call({get_users}, _From, State) ->
    Response = {ok, State},
    {reply, Response, State};

handle_call({get_error}, _From, _State) ->
    error;

handle_call({login,User,Pass}, _From, State) ->
    UserList = State,
    Response = case lists:keysearch(User,1,UserList) of
	false -> {auth_error};
	{value, {LibUser,LibPass,LibUid}} ->
	    if
		((User==LibUser)and(Pass==LibPass)) ->
		    {auth_success,LibUid};
		true ->
		    {auth_error}
	    end
    end,
    {reply, Response, State};

handle_call({register,User,Pass}, _From, State) ->
    UserList = State,
    {Response, NewState} = case lists:keymember(User,1,UserList) of
	true ->
	    {{register_error}, State};
	false ->
	    NextUid = get_new_uid(UserList),
	    NewUserList = [{User,Pass,NextUid}|UserList],
	    save_user_list(NewUserList),
	    {{register_success,NextUid}, NewUserList}
    end,
    {reply, Response, NewState};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%load from file
load_user_list() ->
    %File_name = code:priv_dir(us)++"/"++?USER_LIST_FILE,
    FileName = "./"++?USER_LIST_FILE,
    jlist_to_userlist(decode_json(load_user_list_file(FileName))).

%load file to binary array
load_user_list_file(FileName) ->
    case file:read_file(FileName) of
	{ok, Bin} -> Bin;
	_Else -> <<>>
    end.

%decode binary array to json list
decode_json(Bin_json) ->
    case jsx:is_json(Bin_json) of
	true -> jsx:decode(Bin_json);
	_False -> []
    end.

%transform json rec list to user tuple {USER, PASS, UID}
jrec_to_utuple(InData) ->
    DataMap = maps:from_list(InData),
    USER = binary_to_list(maps:get(<<"user">>, DataMap, <<>>)),
    PASS = binary_to_list(maps:get(<<"pass">>, DataMap, <<>>)),
    UID  = binary_to_list(maps:get(<<"uid">>, DataMap, <<>>)),
    {USER, PASS, UID}.

jlist_to_userlist(JList) ->
    lists:map(fun(X) -> jrec_to_utuple(X) end, JList).

save_user_list(UserList) ->
    %UserList = load_user_list(),
    FileName = "./"++?USER_LIST_FILE,
    save_user_list(FileName, encode_json(userlist_to_jlist(UserList))).

save_user_list(FileName, BinaryData) ->
    file:write_file(FileName, BinaryData).

utuple_to_jrec(InData) ->
    {User,Pass,Uid} = InData,
    [{<<"user">>, list_to_binary(User)},
     {<<"pass">>, list_to_binary(Pass)},
     {<<"uid">>,  list_to_binary(Uid)}].

userlist_to_jlist(UserList) ->
    lists:map(fun(X) -> utuple_to_jrec(X) end, UserList).

encode_json(JList) ->
    jsx:encode(JList).

get_new_uid(UserList) ->
    OnlyUid = lists:map(fun({_User,_Pass,Uid}) -> string:to_integer(Uid) end, UserList),
    MaxUid  = element(1, lists:max(OnlyUid)),
    integer_to_list(MaxUid+1).
