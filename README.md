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

    #select
    curl -X GET http://localhost:8080/abc -i
    
    #insert/update
    curl -X PUT --data "123" http://localhost:8080/abc
    #insert with TTL
    curl -X POST -F 'value=123' -F 'ttl=100' http://localhost:8080/abc
    
    #delete
    curl -X DELETE http://localhost:8080/abc -i
