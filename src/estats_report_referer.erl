%% Copyright
-module(estats_report_referer).
-include("include/click_info.hrl").

-behaviour(estats_gen_report).

-export([handle_click/2, handle_report/2, handle_info/1]).

handle_click(Click, Report) ->
  Step = [ 1, estats_click:uniq_step(Click) ],
  { _ , Hash } = estats_report:map_save(Report, infinity, [ b, date:to_week_number(Click#click_info.date) ], Click#click_info.domain ),
  estats_report:counter_inc(Report, [ a, Click#click_info.date, Click#click_info.affiliate_id ], [ Hash, Click#click_info.offer_id ] , Step ),
  estats_report:counter_inc(Report, [ c, Click#click_info.date, Click#click_info.affiliate_id ], [ Click#click_info.http_referer_hash, Click#click_info.offer_id ] , Step ),
  ok.

handle_info(domain_top) ->
  { ok, [ order_pos, affiliate_id ],
    fun({ Order_pos, _Affiliate }, List) ->
      Data = lists:reverse(estats_report:sort_by_value(Order_pos, estats_report:value_sum(List))),
      { group_by , [ domain ], Data }
    end
  };

handle_info(referer_top) ->
  { ok, [ order_pos, affiliate_id ],
    fun({Order_pos, _Affiliate }, List) ->
      Data = lists:reverse(estats_report:sort_by_value(Order_pos, estats_report:value_sum(List))),
      {group_by, [ referer_hash ], Data }
    end
  }.

handle_report({domain_top, Period, { _Order_pos, Affiliate } }, Report) ->
  HashList = estats_report:index_get_all(Report, [ b, lists:usort(lists:map(
    fun(Date) ->
      date:to_week_number(Date)
    end, Period)) ], []),
  Domains = dict:from_list([ {Hash, Value} || { [ _ , _, Hash ], Value } <- estats_report:map_load_list(Report, HashList) ]),
  Keys = estats_report:index_get_all(Report, [ a, Period , Affiliate ]),
  Counters = estats_report:counters_list_get(Report, Keys),
  { ok, lists:map(fun({[ _, _, _, Hash], Value }) ->
    { [ dict:fetch(Hash, Domains) ], Value }
  end, Counters)  };

handle_report({referer_top, Period, { _Order_pos, Affiliate } }, Report) ->
  Keys = estats_report:index_get_all(Report, [ c, Period , Affiliate ]),
  Counters = [ { Hash, Value} || { [ _,_,_, Hash ], Value } <- estats_report:counters_list_get(Report, Keys) ],
  { ok, Counters }.
