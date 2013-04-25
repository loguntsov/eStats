%% Copyright
-module(t).

-include("include/click_info.hrl").
-include("include/offer_info.hrl").

%% API
-export([t/0, t0/2, t1/3, t2/3]).

t() ->
  erlang:spawn_link(fun() ->
    error_logger:info_report({tc, timer:tc(fun() ->
        clicks(estats_offer_server:pid(), 10000)
    end)})
  end).

t0(Date, Query) ->
  estats_offer_server:report(estats_offer_server:pid(), estats_report_count, affiliates_by_offer, Date , { Query }).

t1(Date, Offer, Affiliate) ->
  estats_offer_server:report(estats_offer_server:pid(), estats_report_count, affiliates_hour_count, Date , {Offer, Affiliate}).

t2(Date, Offer, Affiliate) ->
  estats_offer_server:report(estats_offer_server:pid(), estats_report_count, affiliates_day_count, Date , {Offer, Affiliate}).


clicks(_Pid, 0) -> ok;

clicks(Pid, N) ->
  estats_offer_server:click(Pid, random_click()),
  %timer:sleep(1),
  if
    N rem 5000 == 0 ->
      io:format("~p clicks ~n",[N]);
    true -> ok
  end,
  clicks(Pid, N-1).

random_click() ->
  %%Date = { 2013, random:uniform(3),random:uniform(31) },
  Date = { 2013, 2, 3 },
  case date:is_valid(Date) of
    false -> random_click();
    true ->
      #click_info {
        id = random:uniform(10000),
        offer_id = random:uniform(4),
        offer_url_id = random:uniform(2),
      % banner id (не реализовано)
        affiliate_id = random:uniform(4),
        advertiser_id = random:uniform(2),
        subid = dict:from_list([
          { 1, random_prop({<<"1">>, <<"2">>, <<"3">>, <<"4">>, <<"5">>, <<"6">>, <<"7">>, <<"8">>}) },
          { 2, random_prop({<<"1">>, <<"2">>, <<"3">>, <<"4">>, <<"5">>, <<"6">>, <<"7">>, <<"8">>}) },
          { 3, random_prop({<<"1">>, <<"2">>, <<"3">>, <<"4">>, <<"5">>, <<"6">>, <<"7">>, <<"8">>}) },
          { 4, random_prop({<<"1">>, <<"2">>, <<"3">>, <<"4">>, <<"5">>, <<"6">>, <<"7">>, <<"8">>}) },
          { 5, random:uniform(10000) }
        ]),  % Список subid
        http_referer = << <<"referer.com/hello/world?hi=">>/binary, (erlang:integer_to_binary(random:uniform(100)))/binary>>,
        domain = << <<"referer">>/binary, (erlang:integer_to_binary(random:uniform(10000)))/binary, <<".com">>/binary >>,
        user_agent = <<"Unknown">>,
        ip = <<"10.123.123.54">>,
        year = date:year(Date),
        month = date:month(Date), %random:uniform(2),
        day = date:day(Date), %random:uniform(30),
        day_number = calendar:date_to_gregorian_days(Date),
        day_of_week = calendar:day_of_the_week(Date),
        hour = random:uniform(24) - 1,
        date = Date,
        is_unique = (random:uniform(1000) rem 2) == 0
      }
  end.

random_prop(Tuple) ->
  element(random:uniform(tuple_size(Tuple)), Tuple).


