%% Copyright
-module(estats_redis_sup).

%% API
-export([init/1, start_link/1]).

-behaviour(supervisor).

init(Connections) ->
  { Childs, _ } = lists:mapfoldl(fun(Conn, Index) ->
    { { { redis, Index } , { estats_redis_connection , start_link , [ Conn ] }, permanent, 5000, worker, [] } ,
      Index + 1
    }
  end, 0, Connections),
  { ok, {{one_for_one, 1, 1000}, Childs }}.

start_link(Connections) ->
  supervisor:start_link(?MODULE, Connections).
