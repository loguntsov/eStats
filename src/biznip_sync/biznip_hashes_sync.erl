%% Copyright
-module(biznip_hashes_sync).

%% API
-export([start_link/1, execute/2, execute/3]).

start_link(Args) ->
  { Date, _ } = calendar:local_time(),

  Pid = spawn_link(?MODULE, execute, Args ++ [ date:next_days(Date, -1) ]),
  {ok, Pid}.

execute(Table, Date) ->
  execute(Table, <<"">>, Date).

execute({hour, Table} , Where, {Year, Month, _Day} = Date) ->
  Where0 = and_where(Where),
  Rows = mysql:select([ <<"SELECT affiliate_id, offer_url_id, advertiser_id, clicks_total, clicks_unique, offer_id, hash FROM `">>, table(hour, Table), <<"` WHERE date = ?">>, Where0 ], [ binary_to_list(date:to_sql_binary(Date))]),
  Ets = ets:new(none, [ set, private]),
  lists:foldl(fun(ValueList, _) ->
    Row = list_to_tuple(ValueList),
    Key = hash:encode([element(7, Row), element(1, Row), element(2, Row), Date ]),  %%hash:affiliate_id:offer_url_id:date
    try
      ets:update_counter(Ets, Key, [{ 4, element(4, Row) }, { 5, element(5, Row) }])
    catch
      error:badarg ->
        ets:insert(Ets, { Key, Row })
    end,
    ok
  end, ok, Rows),
  Weekday = integer_to_binary(calendar:day_of_the_week(Date)),
  DateSql = date:to_sql_binary(Date),
  DateMonthSql = date:to_sql_binary({Year, Month, 1}),

  ets:foldl(fun({Key, Data}, _) ->
    mysql:counter_update(table(day, Table), [
      { <<"id">>, Key },
      { <<"hash">>, element(7, Data) },
      { <<"date">>, DateSql },
      { <<"affiliate_id">>, integer_to_binary(element(1, Data)) },
      { <<"offer_url_id">>, integer_to_binary(element(2, Data)) },
      { <<"advertiser_id">>, integer_to_binary(element(3, Data)) },
      { <<"weekday">>, Weekday },
      { <<"offer_id">>, integer_to_binary(element(6, Data)) }
    ], [
      { <<"clicks_total">>, element(4, Data) },
      { <<"clicks_unique">>, element(5, Data) }
    ]),

    MonthId = hash:encode([element(7, Data), element(1, Data), element(2, Data), Year, Month ]),  %%hash:affiliate_id:offer_url_id:year:month
    mysql:counter_update(table(month, Table), [
      { <<"id">>, MonthId },
      { <<"hash">>, element(7, Data) },
      { <<"date">>, DateMonthSql },
      { <<"affiliate_id">>, integer_to_binary(element(1, Data)) },
      { <<"offer_url_id">>, integer_to_binary(element(2, Data)) },
      { <<"advertiser_id">>, integer_to_binary(element(3, Data)) },
      { <<"offer_id">>, integer_to_binary(element(6, Data)) }
    ], [
      { <<"clicks_total">>, element(4, Data) },
      { <<"clicks_unique">>, element(5, Data) }
    ]),

    ok
  end, ok, Ets),
  ets:delete(Ets),
  delete_not(table(hour, Table), Where),
  ok;

execute({day, Table}, Where, {Year, Month, _Day} = Date) ->
  Where0 = and_where(Where),
  Rows = mysql:select([ <<"SELECT affiliate_id, offer_url_id, advertiser_id, clicks_total, clicks_unique, offer_id, hash FROM `">>, table(day, Table), <<"` WHERE date = ?">>, Where0 ], [ binary_to_list(date:to_sql_binary(Date))]),
  DateMonthSql = date:to_sql_binary({Year, Month, 1}),
  lists:foldl(fun(ValueList, _) ->
    Data = list_to_tuple(ValueList),
    MonthId = hash:encode([element(7, Data), element(1, Data), element(2, Data), Year, Month ]),  %%affiliate_id:offer_url_id:date
    mysql:counter_update(table(month, Table), [
      { <<"id">>, MonthId },
      { <<"hash">>, element(7, Data) },
      { <<"date">>, DateMonthSql },
      { <<"affiliate_id">>, integer_to_binary(element(1, Data)) },
      { <<"offer_url_id">>, integer_to_binary(element(2, Data)) },
      { <<"advertiser_id">>, integer_to_binary(element(3, Data)) },
      { <<"offer_id">>, integer_to_binary(element(6, Data)) }
    ], [
      { <<"clicks_total">>, element(4, Data) },
      { <<"clicks_unique">>, element(5, Data) }
    ]),

    ok
  end, ok, Rows),
  delete_not(table(day, Table), Where),
  ok.

table(Type, Table) when is_binary(Table), is_atom(Type) ->
  list_to_binary([Table, <<"_">>, atom_to_binary(Type, utf8)]).


and_where(<<"">>) -> <<"">>;
and_where([]) -> <<"">>;
and_where(Where) when is_list(Where) -> and_where(list_to_binary(Where));
and_where(Where) when is_binary(Where) -> list_to_binary([ <<" AND ">> , Where]).


delete_not(_Table, <<"">>) -> ok;
delete_not(_Table, []) -> ok;
delete_not(Table, Where) when is_binary(Table), is_binary(Where) ->
  mysql:q(<<"DELETE FROM ? WHERE NOT ( ? )">>, [ Table, Where ]).
