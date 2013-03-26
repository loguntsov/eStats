%% Copyright
-module(estats_report_total).

-include("include/click_info.hrl").

-behaviour(estats_gen_report).

-export([handle_click/2, handle_info/1, handle_report/2 ]).

handle_click(Click, Report) ->
  Step = [ 1, estats_click:uniq_step(Click) ],
  Date = Click#click_info.date,
  estats_report:counter_inc(Report, [ a ], [ Date, Click#click_info.affiliate_id ], Step),
  estats_report:counter_inc(Report, [ b, Date ], [ Click#click_info.offer_id ], Step),
  ok.

handle_info(day) ->
  { ok, [ subtype, value ], group_fun(fun(Date) -> Date end) };

handle_info(month) ->
  { ok, [ subtype, value ], fun({Period, QueryTuple}) ->
    Month  = lists:usort([{date:year(Date), date:month(Date) } || Date <- Period]),
    Dates = [ { Y, M, D } || { Y, M } <- Month, D <- lists:seq(1,31), date:is_valid({Y, M, D}) ],
    { Dates , QueryTuple }
  end, group_fun(fun(Date) -> date:start_month(Date) end) };

handle_info(week) ->
  { ok, [ subtype, value ], fun({Period, QueryTuple}) ->
    Week  = lists:usort([date:start_week(Date) || Date <- Period]),
    Dates = [ date:next_days(Date, D) || Date <- Week, D <- lists:seq(0,6) ],
    { Dates , QueryTuple }
  end, group_fun(fun(Date) -> date:start_week(Date) end) }.


group_fun(KeyFun) ->
  fun(_, ReportData) ->
    Data = estats_report:format_tuple(dict:to_list(lists:foldl(fun({Date, Value}, Dict) ->
      Key = KeyFun(Date), % {date:year(Date), date:month(Date), 1},
      OldValue = case dict:find(Key, Dict) of
        { ok, V } -> V;
        error -> 0
      end,
      dict:store(Key, estats_counter:step_sum(OldValue, Value), Dict)
    end,dict:new(), ReportData)),[ date ]),
    estats_report:group([<<"date">>], Data)
  end.


get_clicks_by_date_list(Report, Period, Keys) when is_list(Period) ->
  estats_report:key_transform(fun([ _ , Date | _ ]) -> Date end,
    estats_report:counters_list_get(Report, estats_report:subkey_list(Keys))
  ).

handle_report({_, Period, { <<"total">> , _ } }, Report) ->
  { ok, get_clicks_by_date_list(Report, Period, [ a , Period ]) };

handle_report({_, Period, { <<"affiliate">>, Affiliate_id } }, Report) ->
  { ok, get_clicks_by_date_list(Report, Period, [ a , Period, Affiliate_id ]) };

handle_report({_, Period, { <<"offer">>, Offer_id } }, Report) ->
  { ok, get_clicks_by_date_list(Report, Period, [ b , Period, Offer_id ]) }.
