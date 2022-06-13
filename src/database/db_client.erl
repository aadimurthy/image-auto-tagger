-module(db_client).

-include_lib("epgsql/include/epgsql.hrl").

-export([connect/2, disconnect/1, query/3]).

connect(Options, QueryTimeout) ->
    case epgsql:connect(db_opts(Options)) of
        {ok, Pid} ->
            epgsql:squery(Pid, [<<"SET statement_timeout=">>, integer_to_binary(QueryTimeout)]),
            epgsql:squery(Pid, <<"SET standard_conforming_strings=off">>),
            {ok, Pid};
        Error ->
            Error
    end.

db_opts(Options) ->
    #{host => "localhost",
      username => "postgres",
      password => "password",
      database => "image_auto_tagger",
      port => 5432,
      timeout => 4000}.

disconnect(Connection) ->
    epgsql:close(Connection).

query(Connection, {Statment, Values}, _Timeout) ->
    pgsql_to_rdbms(epgsql:equery(Connection, Statment, Values)).

pgsql_to_rdbms(Items) when is_list(Items) ->
    lists:reverse([pgsql_to_rdbms(Item) || Item <- Items]);
pgsql_to_rdbms({error, #error{codename = unique_violation}}) ->
    {error, duplicate_key};
pgsql_to_rdbms({error, #error{message = Message}}) ->
    {error, unicode:characters_to_list(Message)};
pgsql_to_rdbms({ok, Count}) ->
    {updated, Count};
pgsql_to_rdbms({ok, Count, _Column, Value}) ->
    {updated, Count, Value};
pgsql_to_rdbms({ok, _Columns, Rows}) ->
    {selected, Rows}.
