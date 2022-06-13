-module(fetch_images).

-export([by_id/1, all/0]).

by_id(ImageId) ->
    Tags = db_app:get_tags_by_imageId(ImageId),
    {ImageUri, ImageLabel} = db_app:get_label_and_url_by_imageId(ImageId),
    #{imageUri=> ImageUri, imageLabel =>ImageLabel, tags =>Tags}.


all() ->
    ok.
