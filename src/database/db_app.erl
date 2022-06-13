-module(db_app).

-behaviour(gen_server).

-export([start_link/0]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, query/1,
         code_change/3]).

-record(state, {connection}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

query(Statement) ->
    gen_server:call(?MODULE, Statement).

init([]) ->
    {ok, Conn} = db_client:connect(1, 2),
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