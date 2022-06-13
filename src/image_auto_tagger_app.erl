%%%-------------------------------------------------------------------
%% @doc image-auto-tagger public API
%% @end
%%%-------------------------------------------------------------------

-module(image_auto_tagger_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    {ok, Pid} = image_auto_tagger_sup:start_link(),
    Routes = router:publish(),
    Dispatch = cowboy_router:compile(Routes),
    TransOpts = [{ip, {0, 0, 0, 0}}, {port, 8888}],
    ProtoOpts = #{env => #{dispatch => Dispatch}},
    {ok, _} = db_app:start_link(),
    {ok, _} = cowboy:start_clear(image_auto_tagger, TransOpts, ProtoOpts),
    {ok, Pid}.

stop(_State) ->
    ok.

%% internal functions
