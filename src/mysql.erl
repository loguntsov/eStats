%% Copyright
-module(mysql).

-include("include/logger.hrl").

%% API
-export([start_link/1, counter_update/3, select/1, select/2, q/2]).

-behaviour(supervisor).
-export([init/1]).

-export([worker/2]).

-define(CONNECTIONS, 10).

start_link(Db) ->

  ?INFO_(Db),

  Count = min(proplists:get_value(username, Db, 0), ?CONNECTIONS),

  emysql:add_pool(
    mysql_pool,
    Count,
    proplists:get_value(username, Db, "bz_stat"),
    proplists:get_value(password, Db, "UvqrqXS42hxP3bY2"),
    proplists:get_value(host, Db, "biznip.local"),
    proplists:get_value(port, Db, 3306),
    proplists:get_value(database, Db, "bz_stat_biznip"),
    utf8),

  supervisor:start_link({local, ?MODULE}, ?MODULE, {Count}).

select(Query) ->
  select(Query, []).

select(Query, Args) when is_list(Query) ->
  select(list_to_binary(Query),Args);

select(Query, Args) when is_binary(Query) ->
  Result = emysql:execute(mysql_pool, Query, Args),
  result_packet = element(1, Result),
  element(4, Result).

q(Query, Args) when is_list(Query) ->
  q(list_to_binary(Query), Args);

q(Query, Args) when is_binary(Query) ->
  Result = emysql:execute(mysql_pool, Query, Args),
  ok_packet = element(1, Result),
  ok.

-spec counter_update(binary(), [ { binary(), binary() } ], [ { binary(), integer() } ] ) -> ok.
counter_update(Table, Keys, Increment) ->
  CounterBinary = lists:map(fun({ Col, Value }) when is_binary(Col), is_integer(Value)->
    {Col, integer_to_binary(Value) }
  end, Increment),

  KeyBinary = lists:foldl(fun
    ({ Col, Value }, <<>>) when is_binary(Col), is_binary(Value) ->
      list_to_binary([ Col, <<"='">>, Value, <<"'">>]);
    ({ Col, Value }, Binary) when is_binary(Col), is_binary(Value) ->
      list_to_binary([ Binary, <<",">>, Col, <<"='">>, Value, <<"'">> ])
  end, <<>>, CounterBinary ++ Keys),

  IncrementBinary = list_to_binary(lists:foldl(fun
    ({ Col, Value }, []) ->
      [<<"`">>, Col, <<"`=`">>, Col, <<"`+">>, Value];
    ({ Col, Value }, Binary) ->
      [ <<"`">>, Col, <<"`=`">>, Col, <<"`+">>, Value, <<",">> | Binary ]
  end, [], CounterBinary)),

  Query = list_to_binary([ <<"INSERT INTO `">>, Table, <<"` SET ">>, KeyBinary, <<" ON DUPLICATE KEY UPDATE ">>, IncrementBinary ]),

  %?INFO_(Query),

  async_query(Query),
  ok.

init({Count}) ->
  {ok, {{simple_one_for_one, 10, 10}, [
    { none, { ?MODULE, worker, [Count] }, transient, 600 * 1000 , worker, [?MODULE] }
  ]}}.

worker(TotalCount, Query) ->
  if
    TotalCount == 0 ->
      ignore;
    true ->
      Pid = spawn_link(fun() ->
        receive
          { query , Q } ->
            Result = emysql:execute(mysql_pool, Q),
            try
              ok_packet = element(1, Result)
            catch
              error:{badmatch, _} ->
                erlang:error({badarg,binary_to_list(Q),element(5,Result)})
            end
        end
      end),
      Pid ! { query, Query },
      {ok, Pid }
  end.

async_query(Query) ->
  Count = length(supervisor:which_children(?MODULE)),
  if
     Count > ?CONNECTIONS ->
      timer:sleep(10),
      async_query(Query);
    true ->
      supervisor:start_child(?MODULE, [Query])
  end.
