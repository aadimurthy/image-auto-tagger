%%%-------------------------------------------------------------------
%% @doc image-auto-tagger public API
%% @end
%%%-------------------------------------------------------------------

-module(image-auto-tagger_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    image-auto-tagger_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
