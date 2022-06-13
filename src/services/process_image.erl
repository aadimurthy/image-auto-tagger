-module(process_image).

-export([start/3]).

start(Uri, IsDetectionEnabled, Label) ->
    {updated, 1, [{ImageId}]} = db_app:store_uploaded_image_info(Uri, Label),
    #{<<"tags">> := TagInfoList} = imagga_client:tag_image(Uri),
    [process_each_tag(TagInfo, ImageId) || TagInfo <- TagInfoList].

process_each_tag(TagInfo, ImageId) ->
    #{<<"confidence">> := Confidence, <<"tag">> := Tag} = TagInfo,
    Langugue = hd(maps:keys(Tag)),
    TagName = hd(maps:values(Tag)),
    {updated, 1, [{TagId}]} = db_app:store_tag_info(TagName, Langugue),
    db_app:stote_image_tag_associations(ImageId, TagId, Confidence).
