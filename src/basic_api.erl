-module('basic_api').
-export([init/2]).

init(Req, Opts) ->
    Req2 = cowboy_req:reply(200,
       #{<<"content-type">> => <<"text/plain">>},
        <<"Hello Erlang!">>,
        Req),
    {ok, Req2, Opts}.