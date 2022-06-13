-module(fetch_images).

-export([by_id/1, all/0, by_tags/1]).

by_id(ImageId) ->
    Tags = db_app:get_tags_by_imageId(ImageId),
    {ImageUri, ImageLabel, IsDetectionEnabled} = db_app:get_image_info_by_imageId(ImageId),
    #{imageUri => ImageUri,
      imageLabel => ImageLabel,
      isDetectionEnabled => IsDetectionEnabled,
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
    TaggedRecords = db_app:get_tags_group_by_image(),
    UntaggedRecords = db_app:get_untagged_images(),
    TaggedResult =
        [#{imageId => ImageID,
           imageUri => ImageUri,
           imageLable => ImageLable,
           isDetectionEnabled => true,
           tags => Tags}
         || {Tags, ImageUri, ImageLable, Confidence, ImageID} <- TaggedRecords],

    UntaggedResult =
        [#{imageId => ImageID,
           imageUri => ImageUri,
           imageLable => ImageLable,
           isDetectionEnabled => true,
           tags => []}
         || {ImageID, ImageUri, ImageLable} <- UntaggedRecords],
    TaggedResult ++ UntaggedResult.
