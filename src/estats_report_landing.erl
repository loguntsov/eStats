%% Copyright
-module(estats_report_landing).

-include("include/click_info.hrl").

-behaviour(estats_gen_report).

-export([handle_click/2, handle_report/2, handle_info/1, handle_mysql_counter/1]).

handle_click(Click, Report) ->
  Step = [ 1, estats_click:uniq_step(Click) ],
  Hour = Click#click_info.hour,
  Date = Click#click_info.date,
  estats_report:counter_inc(Report, [ a, Click#click_info.advertiser_id, Click#click_info.affiliate_id, Click#click_info.offer_id, Click#click_info.offer_url_id, Date ] , [ Hour ], Step ),
  ok.

handle_info(_) -> {error, no_info }.


handle_report(_, _Report) -> { error, no_report }.


handle_mysql_counter({ [ a, Advertiser, Affiliate, Offer_id, Offer_url_id, Date, Hour ] , Clicks_Total, Clicks_Unique }) ->
  Id = hash:encode([Affiliate, Offer_url_id, Date, Hour]),
  mysql:counter_update(<<"clicks_hour">>, [
    { <<"id">>, Id },
    {<<"affiliate_id">>, integer_to_binary(Affiliate)},
    {<<"advertiser_id">>, integer_to_binary(Advertiser)},
    {<<"offer_id">>, integer_to_binary(Offer_id)},
    {<<"offer_url_id">>, integer_to_binary(Offer_url_id)},
    {<<"date">>, list_to_binary(date:to_string(Date))},
    {<<"hour">>, integer_to_binary(Hour)},
    {<<"weekday">>, integer_to_binary(calendar:day_of_the_week(Date))}
  ],[
    {<<"clicks_total">>, Clicks_Total},
    {<<"clicks_unique">>, Clicks_Unique}
  ]),
  ok.

