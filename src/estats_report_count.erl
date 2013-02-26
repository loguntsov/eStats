%% Copyright
-module(estats_report_count).





-include("include/click_info.hrl").

%% API
-export([]).

-behaviour(estats_gen_report).

-export([handle_click/2, handle_report/2]).

handle_click(Click, Report) ->
  Hour = Click#click_info.hour,
  estats_gen_report:counter_inc(Report, [ Click#click_info.date, Click#click_info.offer_id ] , [ Click#click_info.affiliate_id, Hour ]),
  estats_gen_report:counter_inc(Report, [ Click#click_info.date, Click#click_info.offer_id ] , [ 0 , Hour ]),

  ok.

handle_report({affiliates_of_offer, Period, Offer }, Report) when is_list(Period) ->
  {ok, 0,
    [ X || X <- lists:usort(
      [ Affiliate || [ _, _, Affiliate ] <- estats_gen_report:index_get_all(Report, [Period, Offer], [])]
    ), X > 0]
  };

handle_report({affiliates_hour_count, Period, { Offer, Affiliate } }, Report) when is_list(Period) ->
  {ok, 3,
    [ { estats_gen_report:subkey_swap(Key, [2, 1, 4, 3]), Value } || {Key, Value} <-
        estats_gen_report:counters_list_get(Report,
          estats_gen_report:subkey_list(
            [Period, Offer, Affiliate, lists:seq(0,23) ])
        )
    ]
  };

handle_report({affiliates_day_count, Period, { Offer, Affiliate } }, Report) when is_list(Period) ->
  {ok, 2,
    [ { estats_gen_report:subkey_swap(Key, [2, 1, 3]), Value } || {Key, Value} <-
      estats_gen_report:counters_list_get(Report,
        estats_gen_report:subkey_list(
          [Period, Offer, Affiliate ])
      )
    ]
  }.

