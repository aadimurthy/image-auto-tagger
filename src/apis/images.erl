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
    case cowboy_req:method(Req) of
        <<"POST">> ->
            post(Req, State);
        <<"GET">> ->
            case cowboy_req:binding(image_id, Req) of
                undefined ->
                    get(Req, State);
                ImageId ->
                    get(Req, ImageId, State)
            end
    end.

post(Req, State) ->
    Resp = cowboy_req:set_resp_body(<<"{\"a\":\"b\"}">>, Req),
    cowboy_req:reply(201, Resp),
    {stop, Resp, State}.

get(Req, State) ->
    #{id := ID, lang := Lang} = cowboy_req:match_qs([{objects, [], <<"1">>}], Req),
    {<<"{\"a\":\"b\"}">>, Req, State}.

get(Req, ImageId, State) ->
    {<<"{\"a\":\"b\"}">>, Req, State}.
