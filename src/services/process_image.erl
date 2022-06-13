-module(process_image).

-export([start/3]).

start(Uri, true, Label) ->
    {updated, 1, [{ImageId}]} = db_app:store_uploaded_image_info(Uri, Label, true),
    #{<<"tags">> := TagInfoList} = imagga_client:tag_image(Uri),
    [process_each_tag(TagInfo, ImageId) || TagInfo <- TagInfoList],
    fetch_images:by_id(list_to_binary(integer_to_list(ImageId)));
start(Uri, false, Label) ->
    {updated, 1, [{ImageId}]} = db_app:store_uploaded_image_info(Uri, Label, false),
    fetch_images:by_id(list_to_binary(integer_to_list(ImageId))).

process_each_tag(TagInfo, ImageId) ->
    #{<<"confidence">> := Confidence, <<"tag">> := Tag} = TagInfo,
    Langugue = hd(maps:keys(Tag)),
    TagName = hd(maps:values(Tag)),
    {updated, 1, [{TagId}]} = db_app:store_tag_info(TagName, Langugue),
    db_app:stote_image_tag_associations(ImageId, TagId, Confidence).
