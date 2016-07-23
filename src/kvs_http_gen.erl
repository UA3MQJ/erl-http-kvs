%% -*- coding: utf-8 -*-
-module(kvs_http_gen).
-behaviour(gen_server).

%%API
-export([start_link/0]).

%%GEN_SERVER callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
  terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(TCP_HTTP_OPTIONS, [{active, true},binary,{packet,http},
                           {buffer,4096},{delay_send,false},
                           {reuseaddr,true}]).

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
  Pid = spawn(fun() -> start_http(8080) end),
  State = Pid,
  {ok, State}.

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

%% HTTP Srv
start_http(Port) ->
  io:format("~s Start HTTP on port ~w PID=~w~n", [current_datetime(), Port, self()]),
  case gen_tcp:listen(Port, ?TCP_HTTP_OPTIONS) of
    {ok, LSocket} ->
      start_http_server(LSocket);
    {error, Reason} ->
      {error, Reason}
  end.

start_http_server(LS)->
  case gen_tcp:accept(LS) of
    {ok,S} ->
      io:format("~s HTTP ACCEPTED~n", [current_datetime()]),
      PID = spawn(fun()-> http_loop(S, "", "") end),
      gen_tcp:controlling_process(S,PID);
    Other ->
      io:format("~s HTTP ACCEPT RETURNED ~w - disconnect~n", [current_datetime(), Other])
  end,
  start_http_server(LS).

http_loop(Sock, Method, Patch) ->
  receive
    {http, Sock, Data} ->
      %%io:format("~s HTTP RCVD ~w~n", [current_datetime(), Data]),
      case Data of
        {http_request, HttpMethod, HttpUri, _HttpVersion} ->
          {abs_path, AbsPath} = HttpUri,
          %io:format("~s HTTP REQUEST~nHttpMethod=~w~nHttpUri=~w~nAbsPath=~s~nHttpVersion=~w~n", [current_datetime(), HttpMethod, HttpUri, AbsPath, HttpVersion]),
          [_|NewPatch] = AbsPath,
          http_loop(Sock, HttpMethod, NewPatch);
        {http_header, _Int, _HttpField, _Reserved, _Value} ->
          %io:format("~s HTTP HEADER~nInt=~w, HttpField=~s, Reserved=~s, Value=~s~n", [current_datetime(), Int, HttpField, Reserved, Value]),
          http_loop(Sock, Method, Patch);
        http_eoh ->
          %io:format("~s HTTP end of HEADER: ~s ~s ~n", [current_datetime(), Method, Patch]),

          case (Method) of
            'PUT'    -> handle_put(Sock, Patch);    %INSERT
            'POST'   -> handle_post(Sock, Patch);   %UPDATE
            'DELETE' -> handle_delete(Sock, Patch);     %DELETE
            'GET'    -> handle_get(Sock, Patch);        %SELECT
            _ -> send_unsupported_error(Sock)
          end;
        Other->
          io:format("~s HTTP Other ~w~n", [current_datetime(), Other]),
          http_loop(Sock, Method, Patch)
      end;
    {tcp_closed,Sock} ->
      io:format("~s HTTP CLOSED~n", [current_datetime()]),
      ok
  end.

%% kvs_db_gen:start_link().
%% kvs_http_gen:start_link().

handle_post(Sock, Patch) -> handle_put(Sock, Patch).

handle_put(Sock, Patch) ->
  inet:setopts(Sock, [{active, false}, {packet, raw}]),
  {ok, Body}=gen_tcp:recv(Sock, 0), %% string:to_integer(Length)),
  kvs_db_gen:insert(Patch, Body),
  gen_tcp:send(Sock, "HTTP/1.1 202 Accepted\r\nConnection: close\r\nContent-Type: text/html; charset=UTF-8\r\nCache-Control: no-cache\r\n\r\n"),
  gen_tcp:close(Sock).

handle_delete(Sock, Patch) ->
  kvs_db_gen:delete(Patch),
  gen_tcp:send(Sock, "HTTP/1.1 200 OK\r\nConnection: close\r\nContent-Type: text/html; charset=UTF-8\r\nCache-Control: no-cache\r\n\r\n"),
  gen_tcp:close(Sock).

handle_get(Sock, Patch) ->
  Dat = kvs_db_gen:select(Patch),
  {Header, Body} = case Dat of
      null -> {"HTTP/1.1 404 Not found\r\nConnection: close\r\nContent-Type: text/plain; charset=UTF-8\r\nCache-Control: no-cache\r\n\r\n", ""};
      _ELSE -> {"HTTP/1.1 200 OK\r\nConnection: close\r\nContent-Type: text/plain; charset=UTF-8\r\nCache-Control: no-cache\r\n\r\n", Dat}
  end,
  gen_tcp:send(Sock, Header),
  gen_tcp:send(Sock, Body),
  gen_tcp:close(Sock).

send_unsupported_error(Sock) ->
  gen_tcp:send(Sock, "HTTP/1.1 405 Method Not Allowed\r\nConnection: close\r\nAllow: POST\r\nContent-Type: text/html; charset=UTF-8\r\nCache-Control: no-cache\r\n\r\n"),
  gen_tcp:close(Sock).

%% LOG
current_epoch() ->
  {Mega,Sec,Micro} = erlang:now(),
  (Mega*1000000+Sec)*1000000+Micro.

timestamp(Now) ->
  {_, _, Micros} = Now,
  {{YY, MM, DD}, {Hour, Min, Sec}} = calendar:now_to_local_time(Now),
  io_lib:format("~4..0w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w.~6p",
    [YY,MM,DD,Hour,Min,Sec,Micros]).

current_datetime() ->
  Epoch = lists:sublist(integer_to_list(current_epoch()), 13),
  io_lib:format("~s (~s)", [timestamp(erlang:now()), Epoch]).