erl-http-kvs
=====

Erlang HTTP KVS

Build
-----

    $ rebar3 compile

Start
-----

    erl
    1> application:start(kvs).

Test
-----

    curl -X GET http://localhost:8080/abc -i
    curl --data "123" http://localhost:8080/abc
    curl -X DELETE http://localhost:8080/abc -i
