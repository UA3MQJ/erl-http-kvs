-module(rest_handler).

-export([init/3]).
-export([content_types_provided/2]).
-export([get_data/2, put_data/2]).
-export([allowed_methods/2, delete_resource/2, delete_completed/2]).
-export([resource_exists/2, content_types_accepted/2]).

init(_, _Req, _Opts) ->
	{upgrade, protocol, cowboy_rest}.

allowed_methods(Req, State) ->
	{[<<"GET">>, <<"PUT">>, <<"POST">>, <<"DELETE">>], Req, State}.

content_types_provided(Req, State) ->
	{[{{<<"text">>, <<"html">>, '*'}, get_data}], Req, State}.

content_types_accepted(Req, State) ->
	{[
		{{<<"application">>, <<"x-www-form-urlencoded">>, '*'}, put_data}
	 ], Req, State }.

get_data(Req, State) ->
	Path = get_path(Req),
	Dat = kvs_db_gen:select(Path),
	Body = case Dat of
										 null -> <<>>;
										 _ELSE -> Dat
									 end,
	{Body, Req, State}.

put_data(Req, State) ->
	Path = get_path(Req),
	{ok, Body, _Req1} = cowboy_req:body(Req),
	kvs_db_gen:insert(Path, Body),
	Req2 = cowboy_req:set_resp_body(Body, Req),
	{true, Req2, State}.

delete_resource(Req, State) ->
	Path = get_path(Req),
	kvs_db_gen:delete(Path),
	{true, Req, State}.

delete_completed(Req, State) ->
	{false, Req, State}.

resource_exists(Req, State) ->
	Path = get_path(Req),
	Is_Exist = kvs_db_gen:exist(Path),
	{Is_Exist, Req, State}.

get_path(Req) ->
	{<<_,Path/binary>>, _} = cowboy_req:path(Req),
	Path.

%%get_method(Req) ->
%%	{<<Method/binary>>, _} = cowboy_req:method(Req),
%%	Method.