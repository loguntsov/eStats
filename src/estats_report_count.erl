%% Copyright
-module(estats_report_count).





-include("include/click_info.hrl").

%% API
-export([]).

-behaviour(estats_gen_report).

-export([handle_click/2, handle_report/2, handle_info/1]).

handle_click(Click, Report) ->
  Hour = Click#click_info.hour,
  estats_report:counter_inc(Report, [ offer_by_affiliate, Click#click_info.date, Click#click_info.affiliate_id ] , [ Click#click_info.offer_id, Hour ]),
  estats_report:counter_inc(Report, [ offer_by_affiliate, Click#click_info.date, 0 ] , [ Click#click_info.offer_id , Hour ]),
  estats_report:index_add(Report, [ affiliate_by_offer, Click#click_info.date, Click#click_info.offer_id ] , [ Click#click_info.affiliate_id ]),
  ok.

handle_info(affiliates_by_offer) ->
  { ok,
    [ offer_id ],
    [ affiliate_id ]
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

