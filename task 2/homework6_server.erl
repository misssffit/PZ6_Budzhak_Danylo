-module(homework6_server).
-behaviour(gen_server).

-export([start_link/0, create/1, insert/3, insert/4, lookup/2, delete_obsolete/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% API
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

create(TableName) ->
    gen_server:call(?MODULE, {create, TableName}).

insert(TableName, Key, Value) ->
    gen_server:call(?MODULE, {insert, TableName, Key, Value}).

insert(TableName, Key, Value, TTL) ->
    gen_server:call(?MODULE, {insert, TableName, Key, Value, TTL}).

lookup(TableName, Key) ->
    gen_server:call(?MODULE, {lookup, TableName, Key}).

delete_obsolete(TableName) ->
    gen_server:cast(?MODULE, {delete_obsolete, TableName}).

%% Callbacks
init([]) ->
    {ok, #{}}.

handle_call({create, TableName}, _From, State) ->
    ets:new(TableName, [named_table, public, set]),
    {reply, ok, State};

handle_call({insert, TableName, Key, Value}, _From, State) ->
    ets:insert(TableName, {Key, Value, infinity}),
    {reply, ok, State};

handle_call({insert, TableName, Key, Value, TTL}, _From, State) ->
    Expiry = calendar:datetime_to_gregorian_seconds(calendar:universal_time()) + TTL,
    ets:insert(TableName, {Key, Value, Expiry}),
    {reply, ok, State};

handle_call({lookup, TableName, Key}, _From, State) ->
    case ets:lookup(TableName, Key) of
        [{Key, Value, Expiry}] ->
            CurrentTime = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
            if
                Expiry == infinity; Expiry > CurrentTime -> {reply, Value, State};
                true -> {reply, undefined, State}
            end;
        [] -> {reply, undefined, State}
    end.

handle_cast({delete_obsolete, TableName}, State) ->
    CurrentTime = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
    ets:select_delete(TableName, [{{'$1', '_', '$2'}, [{'<', '$2', CurrentTime}], ['$_']}]),
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
