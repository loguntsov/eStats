%% Copyright
-module(estats_report_subid).

-include("include/click_info.hrl").

-behaviour(estats_gen_report).

-export([handle_click/2, handle_report/2, handle_info/1]).

-define(SUBID_LIMIT_COUNT, 1000).

handle_click(Click, Report) ->
  Step = [ 1, estats_click:uniq_step(Click) ],
  dict:map(fun(Index, Subid) ->
    case estats_report:map_save(Report, ?SUBID_LIMIT_COUNT, [ Click#click_info.date, Click#click_info.affiliate_id, Click#click_info.offer_id, Index ], Subid ) of
      limit ->
        ok;
      { Type, Hash } when Type =:= ok; Type =:= exists ->
        estats_report:counter_inc(Report, [ Click#click_info.date, Click#click_info.affiliate_id, Click#click_info.offer_id, Index ] , [ Hash ], Step )
    end,
    ok
  end, Click#click_info.subid),
  ok.

handle_info(list) ->
  { ok,
    [ affiliate_id, offer_id, index ],
    [ hash ]
  };

handle_info(top) ->
  { ok,
    [ order_pos, affiliate_id, offer_id, index ],
    fun({ Order_pos, _Affiliate, _Offer, _Index }, List) ->
      Dict = lists:foldl(fun({[ _, _, _, _, Hash ], Value}, Dict) ->
        El = case dict:find(Hash, Dict) of
          { ok, Dict_value} -> estats_report_subid:step_sum(Dict_value, Value);
          error -> estats_counter:step_sum(Value, [])
        end,
        dict:store(Hash, El, Dict)
      end, dict:new(), List),
      Data = lists:reverse(estats_report:sort_by_value(Order_pos, dict:to_list(Dict))),
      { group_by, [ hash ], Data }
    end
  }.


handle_report({list, Period, { Affiliate, Offer, Index } }, Report) when is_list(Period) ->
  HashList = estats_report:index_get_all(Report, [ Period, Affiliate, Offer, Index ], []),
  SubidList = estats_report:map_load_list(Report, HashList),
  Dict = dict:from_list(lists:zipwith(fun([ _, _, _, _, Hash ], { _ , Value }) ->
    { Hash, Value }
  end, HashList, SubidList)),
  {ok, dict:to_list(Dict) };

handle_report({top, Period, { _Order_pos, Affiliate, Offer, Index } }, Report ) when is_list(Period) ->
  Counters = estats_report:index_get_all(Report, [ Period, Affiliate, Offer, Index ], []),
  { ok, estats_report:counters_list_get(Report, Counters) }.

