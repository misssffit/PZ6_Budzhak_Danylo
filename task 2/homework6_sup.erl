-module(homework6_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    ServerSpec = {homework6_server,
                  {homework6_server, start_link, []},
                  permanent,
                  5000,
                  worker,
                  [homework6_server]},
    {ok, {{one_for_one, 5, 10}, [ServerSpec]}}.
