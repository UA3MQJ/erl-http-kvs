erl-http-kvs
=====

Erlang HTTP KVS

Build
-----

    $ rebar3 compile
    $ rebar3 release

Start
-----

    $ ./_build/default/rel/kvs/bin/kvs -noshell

Test
-----

    curl -X GET http://localhost:8080/abc -i
    curl -X PUT --data "123" http://localhost:8080/abc
    curl -X DELETE http://localhost:8080/abc -i
