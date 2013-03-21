%% Copyright
-module(estats_report_count).

-include("include/click_info.hrl").

-behaviour(estats_gen_report).

-export([handle_click/2, handle_report/2, handle_info/1]).

handle_click(Click, Report) ->
  Is_uniq_step = case Click#click_info.is_unique of
    true -> 1;
    false -> 0
  end,
  Hour = Click#click_info.hour,
  estats_report:counter_inc(Report, [ offer_by_affiliate, Click#click_info.date, Click#click_info.affiliate_id ] , [ Click#click_info.offer_id, Hour ], [ 1, Is_uniq_step ] ),
  estats_report:counter_inc(Report, [ offer_by_affiliate, Click#click_info.date, 0 ] , [ Click#click_info.offer_id , Hour ], [ 1, Is_uniq_step ]),
  estats_report:index_add(Report, [ affiliate_by_offer, Click#click_info.date, Click#click_info.offer_id ] , [ Click#click_info.affiliate_id ]), %% Индекс: аффилы которые льют на оффер
  estats_report:index_add(Report, [ c, Click#click_info.date, Click#click_info.affiliate_id ] , [ Click#click_info.offer_id ]), %% Индекс: офферы на которые льет аффил
  ok.

handle_info(affiliates_by_offer) ->
  { ok,
    [ offer_id ],
    [ affiliate_id ]
  };

handle_info(offers_by_affiliate) ->
  { ok,
    [ affiliate_id ],
    [ offer_id ]
  };

handle_info(affiliates_hour_count) ->
  { ok,
    [ offer_id, affiliate_id ],
    [ offer_id, date, hour, affiliate_id ]
  };

handle_info(affiliates_day_count) ->
  { ok,
    [ offer_id, affiliate_id ],
    [ offer_id, date, affiliate_id ]
  }.


handle_report({affiliates_by_offer, Period, { Offer } }, Report) when is_list(Period) ->
  {ok, [ X || X <- lists:usort(
      [ Affiliate || [ _, _, _, Affiliate ] <- estats_report:index_get_all(Report, [ affiliate_by_offer, Period, Offer ], [])]
    ), X > 0]
  };

handle_report({offers_by_affiliate, Period, { Affiliate } }, Report) when is_list(Period) ->
  {ok, lists:usort(
    [ Offer || [ _, _, _, Offer ] <- estats_report:index_get_all(Report, [ c, Period, Affiliate ], [])]
  )};

handle_report({affiliates_hour_count, Period, { Offer, Affiliate } }, Report) when is_list(Period) ->
  {ok, [ { estats_report:subkey_swap(Key, [4, 2, 5, 3]), Value } || {Key, Value} <-
        estats_report:counters_list_get(Report,
          estats_report:subkey_list(
            [ offer_by_affiliate, Period, Offer, Affiliate, lists:seq(0,23) ])
        )
    ]
  };

handle_report({affiliates_day_count, Period, { Offer, Affiliate } }, Report) when is_list(Period) ->
  {ok, [ { estats_report:subkey_swap(Key, [4, 2, 3 ]), Value } || {Key, Value} <-
      estats_report:counters_list_get(Report,
        estats_report:subkey_list(
          [ offer_by_affiliate, Period, Offer, Affiliate ])
      )
    ]
  }.

