%% Copyright
-module(biznip_clicks_sync).

%% API
-export([start_link/0, execute/1]).

start_link() ->
  { Date, _ } = calendar:local_time(),

  Pid = spawn_link(?MODULE, execute, [ date:next_days(Date, -1) ]),
  {ok, Pid}.

execute({Year, Month, _Day} = Date) ->
  Rows = mysql:select(<<"SELECT affiliate_id, offer_url_id, advertiser_id, clicks_total, clicks_unique, offer_id FROM clicks_hour WHERE date = ?">>, [ binary_to_list(date:to_sql_binary(Date))]),
  Ets = ets:new(none, [ set, private]),
  lists:foldl(fun(ValueList, _) ->
    Row = list_to_tuple(ValueList),
    Key = hash:encode([element(1, Row), element(2, Row), Date ]),  %%affiliate_id:offer_url_id:date
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
    mysql:counter_update(<<"clicks_day">>, [
      { <<"id">>, Key },
      {<<"date">>, DateSql },
      { <<"affiliate_id">>, integer_to_binary(element(1, Data)) },
      { <<"offer_url_id">>, integer_to_binary(element(2, Data)) },
      { <<"advertiser_id">>, integer_to_binary(element(3, Data)) },
      { <<"weekday">>, Weekday },
      { <<"offer_id">>, integer_to_binary(element(6, Data)) }
    ], [
      { <<"clicks_total">>, element(4, Data) },
      { <<"clicks_unique">>, element(5, Data) }
    ]),

    MonthId = hash:encode([element(1, Data), element(2, Data), Year, Month ]),  %%affiliate_id:offer_url_id:year:month
    mysql:counter_update(<<"clicks_month">>, [
      { <<"id">>, MonthId },
      {<<"date">>, DateMonthSql },
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
  ok.
