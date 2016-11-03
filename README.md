Cerly
=====

An Erlang shell utility to make Curl requests, with serialized JSON output.

    1> cerly:cerl("curl -X GET -H 'Content-Type: application/json' -d '{\"test\":\"test\"}' http://localhost:8080").

Build
-----

    $ rebar3 compile
