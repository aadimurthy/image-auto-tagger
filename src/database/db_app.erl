-module(db_app).

-behaviour(gen_server).

-export([start_link/0]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3,
         store_uploaded_image_info/3, store_tag_info/2, stote_image_tag_associations/3,
         get_tags_by_imageId/1, get_image_info_by_imageId/1, get_images_by_tag/1,
         get_tags_group_by_image/0, get_untagged_images/0]).

-record(state, {connection}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

store_uploaded_image_info(Uri, Label, IsDetectionEnabled) ->
    gen_server:call(?MODULE,
                    {"INSERT INTO uploaded_images(image_name, image_url, is_detection_enabled) VALUES($1, $2, $3) RETURNING id ;",
                     [Label, Uri, IsDetectionEnabled]}).

store_tag_info(TagName, Langugue) ->
    gen_server:call(?MODULE,
                    {"INSERT INTO image_tags(tag, langugue) VALUES($1, $2) RETURNING id;",
                     [TagName, Langugue]}).

stote_image_tag_associations(ImageId, TagId, Confidence) ->
    gen_server:call(?MODULE,
                    {"INSERT INTO image_tag_associations(image_id, tag_id, confidence) VALUES($1, $2, $3) RETURNING id;",
                     [ImageId, TagId, Confidence]}).

get_tags_by_imageId(ImageId) ->
    {selected, Tags} =
        gen_server:call(?MODULE,
                        {"SELECT b.tag, c.confidence FROM image_tags b, image_tag_associations c WHERE c.image_id = $1 AND b.id = c.tag_id ORDER BY c.confidence DESC;		",
                         [binary_to_integer(ImageId)]}),
    Tags.

get_images_by_tag(TagName) ->
    {selected, Result} =
        gen_server:call(?MODULE,
                        {" SELECT b.tag, c.confidence, u.image_url, u.image_name, u.id FROM image_tags b, image_tag_associations c, uploaded_images u WHERE c.image_id = u.id AND c.tag_id = b.id AND b.tag = $1 ORDER BY c.confidence DESC;",
                         [TagName]}),
    Result.

get_image_info_by_imageId(ImageId) ->
    {selected, [Result]} =
        gen_server:call(?MODULE,
                        {"SELECT image_url, image_name, is_detection_enabled FROM uploaded_images WHERE id=$1;",
                         [binary_to_integer(ImageId)]}),
    Result.

get_tags_group_by_image() ->
    {selected, Result} =
        gen_server:call(?MODULE,
                        {" select string_agg(b.tag, ', '), u.image_url, u.image_name, u.is_detection_enabled, u.id from image_tags b, image_tag_associations c, uploaded_images u where c.image_id = u.id and b.id = c.tag_id group by u.id, u.image_url, u.image_name;",
                         []}),
    Result.

get_untagged_images() ->
    {selected, Result} =
        gen_server:call(?MODULE,
                        {"SELECT id,image_url, image_name FROM uploaded_images WHERE is_detection_enabled=false;",
                         []}),
    Result.

init([]) ->
    {ok, Conn} = db_client:connect([], 5000),
    {ok, #state{connection = Conn}}.

handle_call(Statement, _From, #state{connection = Conn} = State) ->
    Reply = db_client:query(Conn, Statement, 3000),
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions
