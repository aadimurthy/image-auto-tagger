-module(imagga_client).

-export([tag_image/1]).

tag_image(Uri) ->
    ImaggaUrl = <<"https://api.imagga.com/v2/tags?image_url=">>,
    UrlToReq = <<ImaggaUrl/binary, Uri/binary>>,
    ApiKey = <<"acc_69f5e4d98656f7b">>,
    ApiSecret = <<"bdde61b0abf4072ddcc94cb886663c87">>,
    Options = [{basic_auth, {ApiKey, ApiSecret}}],
    {ok, _StatusCode, _RespHeaders, ClientRef} =
        hackney:request(get, UrlToReq, [], <<>>, Options),
    {ok, Body} = hackney:body(ClientRef),
    #{<<"result">> := Tags} = jsx:decode(Body),
    Tags.
