%% Copyright
-module(estats_report_subid).

-include("include/click_info.hrl").

-behaviour(estats_gen_report).

-export([handle_click/2, handle_report/2, handle_info/1, step_sum/2]).

-define(SUBID_LIMIT_COUNT, 1000).

handle_click(Click, Report) ->
  Is_uniq_step = case Click#click_info.is_unique of
    true -> 1;
    false -> 0
  end,
  dict:map(fun(Index, Subid) ->
    case estats_report:map_save(Report, ?SUBID_LIMIT_COUNT, [ Click#click_info.date, Click#click_info.affiliate_id, Click#click_info.offer_id, Index ], Subid ) of
      limit ->
        ok;
      { Type, Hash } when Type =:= ok; Type =:= exists ->
        estats_report:counter_inc(Report, [ Click#click_info.date, Click#click_info.affiliate_id, Click#click_info.offer_id, Index ] , [ Hash ], [ 1, Is_uniq_step ])
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
          error -> estats_report_subid:step_sum(Value, [])
        end,
        dict:store(Hash, El, Dict)
      end, dict:new(), List),
      Data = lists:reverse(lists:keysort(
        Order_pos+1, [ { Key, Click, Uniq } || { Key, [ Click, Uniq ] } <- dict:to_list(Dict) ]
      )),
      estats_report:group([<<"hash">>], [
        { Key, [ Click, Uniq ] } ||
        { Key, Click, Uniq } <- Data
      ])
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


step_sum([],[]) -> [];
step_sum([], [B | BList]) ->
  [ B | step_sum([], BList) ];
step_sum([A | AList],[]) ->
  [ A | step_sum(AList, []) ];
step_sum([A | AList ] , [ B | BList ]) ->
  [ A + B | step_sum(AList, BList) ];
step_sum(A, List) when not(is_list(A)), is_list(List) ->
  step_sum([A, 0], List);
step_sum(List, B) when not(is_list(B)), is_list(List) ->
  step_sum(List, [B,0]).
