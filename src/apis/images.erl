-module(images).

-export([init/2, allowed_methods/2, content_types_provided/2, content_types_accepted/2,
         handler/2]).

init(Req0, State) ->
    {cowboy_rest, Req0, State}.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"POST">>], Req, State}.

content_types_provided(Req, State) ->
    {[{<<"application/json">>, handler}], Req, State}.

content_types_accepted(Req, State) ->
    {[{<<"application/json">>, handler}], Req, State}.

handler(Req, State) ->
    process_request(cowboy_req:method(Req), Req, State).

process_request(<<"POST">>, Req, State) ->
    Resp = cowboy_req:set_resp_body(<<"{\"a\":\"b\"}">>, Req),
    cowboy_req:reply(201, Resp),
    {stop, Resp, State};
process_request(<<"GET">>, Req, State) ->
    process_get_request(cowboy_req:binding(image_id, Req), Req, State).

process_get_request(undefined, Req, State) ->
    #{objects := Objects} = cowboy_req:match_qs([{objects, [], <<"1">>}], Req),
    {Objects, Req, State};
process_get_request(ImageId, Req, State) ->
    {ImageId, Req, State}.
