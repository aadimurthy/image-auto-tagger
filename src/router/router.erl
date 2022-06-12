-module(router).

-export([publish/0]).

publish() ->
    [{'_', [{"/images/:image_id", images, []}, {"/images/", images, []}]}].
