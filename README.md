Cerly
=====

An Erlang shell utility to make Curl requests, with serialized JSON output.

**GET**:

```erlang
1> cerly:cerly("curl -X GET -H \"Accept: application/json\" https://jsonplaceholder.typicode.com/posts/1").

Note: Recieved Content-Type "application/json" for GET request. This will be silently ignored.
{ok,{[{<<"userId">>,1},
      {<<"id">>,1},
      {<<"title">>,
       <<"sunt aut facere repellat provident occaecati excepturi optio reprehenderit">>},
      {<<"body">>,
       <<"quia et suscipit\nsuscipit recusandae consequuntur expedita et cum\nreprehenderit "...>>}]}}
```

**POST**:

```erlang
145> cerly:cerly("curl -X POST -H \"Content-Type: application/json\" -d \"{\"test\":\"test\"}\" https://hookb.in/veD6WrOX").  

{ok,{[{<<"success">>,true}]}}
```


Compiling
-----
First, clone the repository.

The easiest way to then compile is to install [rebar3](https://www.rebar3.org/) and then run:

    $ rebar3 compile

Running
-----

Assuming everything went well, you should now be able to access the `cerly` module in the Erlang shell. To test this, just run:

    $ rebar3 shell

And then:

```erlang
1> cerly:cerly("curl -X GET http://localhost").     
"<html><body><h1>It works!</h1></body></html>\n"
```

Todo
-----

Only a handful of Curl arguments are supported now and only GET/POST requests.
