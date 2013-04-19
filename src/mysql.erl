%% Copyright
-module(mysql).

-include("include/logger.hrl").

%% API
-export([start/1, counter_update/3]).

start(Db) ->

  ?INFO_(Db),

  emysql:add_pool(
    mysql_pool,
    proplists:get_value(connections, Db, 1),
    proplists:get_value(username, Db, "bz_stat"),
    proplists:get_value(password, Db, "UvqrqXS42hxP3bY2"),
    proplists:get_value(host, Db, "biznip.local"),
    proplists:get_value(port, Db, 3306),
    proplists:get_value(database, Db, "bz_stat_biznip"),
    utf8).

-spec counter_update(binary(), [ { binary(), binary() } ], [ { binary(), integer() } ] ) -> ok.
counter_update(Table, Keys, Increment) ->
  CounterBinary = lists:map(fun({ Col, Value }) when is_binary(Col), is_integer(Value)->
    {Col, integer_to_binary(Value) }
  end, Increment),

  KeyBinary = lists:foldl(fun
    ({ Col, Value }, <<>>) when is_binary(Col), is_binary(Value) ->
      << Col/binary, <<"='">>/binary, Value/binary, <<"'">>/binary >>;
    ({ Col, Value }, Binary) when is_binary(Col), is_binary(Value) ->
      << Binary/binary, <<",">>/binary, Col/binary, <<"='">>/binary, Value/binary, <<"'">>/binary >>
  end, <<>>, Keys ++ CounterBinary),

  IncrementBinary = lists:foldl(fun
    ({ Col, Value }, <<>>) ->
      << <<"`">>/binary, Col/binary, <<"`=`">>/binary, Col/binary, <<"`+">>/binary, Value/binary >>;
    ({ Col, Value }, Binary) ->
      << Binary/binary, <<",`">>/binary, Col/binary, <<"`=`">>/binary, Col/binary, <<"`+">>/binary, Value/binary >>
  end, <<>>, CounterBinary),

  Query = << <<"INSERT INTO `">>/binary, Table/binary, <<"` SET ">>/binary, KeyBinary/binary, <<" ON DUPLICATE KEY UPDATE ">>/binary, IncrementBinary/binary >>,

  ?INFO_(Query),

  emysql:execute(mysql_pool, Query),
  ok.
