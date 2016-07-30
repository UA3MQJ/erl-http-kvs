-module(rest_handler).

-export([init/3]).
-export([content_types_provided/2]).
-export([get_data/2, put_data/2, put_mdata/2]).
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
		{{<<"application">>, <<"x-www-form-urlencoded">>, '*'}, put_data},
		{{<<"multipart">>, <<"form-data">>, '*'}, put_mdata}
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
	io:format("put_data~n", []),
	Path = get_path(Req),
	{ok, Body, _Req1} = cowboy_req:body(Req),
	kvs_db_gen:insert(Path, Body, inf),
	Req2 = cowboy_req:set_resp_body(Body, Req),
	{true, Req2, State}.


multipart(Req, {Value, Ttl}) ->
	case cowboy_req:part(Req) of
		{ok, Headers, Req2} ->
			{ok, Body, Req3} = cowboy_req:part_body(Req2),
			{data, FieldName} = cow_multipart:form_data(Headers),
			{NewValue, NewTtl} = case FieldName of
					<<"value">> -> {Body, Ttl};
					<<"ttl">>   -> {Value, Body};
					_ -> {Value, Ttl}
			end,
			multipart(Req3, {NewValue, NewTtl});
		{done, _Req2} ->
			{Value, Ttl}
	end.

put_mdata(Req, State) ->
	Path = get_path(Req),
	{Value,Ttl} = multipart(Req, {value,ttl}),
	%io:format("~nput_mdata~nkey=~s~n~s~n~s~n", [Path, Value, Ttl]),
	kvs_db_gen:insert(Path, Value, Ttl),
	Req2 = cowboy_req:set_resp_body(Value, Req),
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