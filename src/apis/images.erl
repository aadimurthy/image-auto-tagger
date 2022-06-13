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
    process_post_request(cowboy_req:has_body(Req), Req, State);
process_request(<<"GET">>, Req, State) ->
    process_get_request(cowboy_req:binding(image_id, Req), Req, State).

process_post_request(false, Req, State) ->
    ErrorMsg = jsx:encode([{<<"ErrorMessage">>, <<"Request must have body">>}]),
    Resp = cowboy_req:set_resp_body(ErrorMsg, Req),
    {false, Resp, State};
process_post_request(true, Req, State) ->
    {ok, JsonData, _} = cowboy_req:read_body(Req),
    Data = jsx:decode(JsonData, [{return_maps, false}]),
    [Uri, IsdetectionEnabled, Label] =
        [proplists:get_value(Param, Data, undefined)
         || Param <- [<<"uri">>, <<"IsdetectionEnabled">>, <<"label">>]],

    case Uri of
        undefined ->
            ErrorMsg = jsx:encode([{<<"ErrorMessage">>, <<"Image URI must be supplied">>}]),
            Resp = cowboy_req:set_resp_body(ErrorMsg, Req),
            {false, Resp, State};
        _ ->
            process_image:start(Uri, IsdetectionEnabled, Label),
            Message = jsx:encode([{<<"uri">>, Uri}]),
            Resp = cowboy_req:set_resp_body(Message, Req),
            {false, Resp, State}
    end.

process_get_request(undefined, Req, State) ->
    #{objects := Objects} = cowboy_req:match_qs([{objects, [], <<"1">>}], Req),
    {Objects, Req, State};
process_get_request(ImageId, Req, State) ->
    ImageData = fetch_images:by_id(ImageId),
    {jsx:encode(ImageData), Req, State}.
