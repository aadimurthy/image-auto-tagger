-module(fetch_images).

-export([by_id/1, all/0, by_tags/1]).

by_id(ImageId) ->
    Tags = db_app:get_tags_by_imageId(ImageId),
    {ImageUri, ImageLabel} = db_app:get_label_and_url_by_imageId(ImageId),
    #{imageUri => ImageUri,
      imageLabel => ImageLabel,
      tags => Tags}.

by_tags(Tags) ->
    TagsList = string:tokens(binary_to_list(Tags), ","),
    Records =
        lists:foldl(fun(Tag, Result) -> Result ++ db_app:get_images_by_tag(Tag) end,
                    [],
                    TagsList),
    [#{imageId => ImageID,
       imageUri => ImageUri,
       imageLable => ImageLable,
       confidence => Confidence,
       tag => Tag}
     || {Tag, Confidence, ImageUri, ImageLable, ImageID} <- Records].

all() ->
    Records = db_app:get_all_tags(),
    [#{imageId => ImageID,
       imageUri => ImageUri,
       imageLable => ImageLable,
       confidence => Confidence,
       tag => Tag}
     || {Tag, Confidence, ImageID, ImageLable, ImageUri, _} <- Records].
