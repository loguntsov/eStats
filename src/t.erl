%% Copyright
-module(t).

-include("include/click_info.hrl").
-include("include/offer_info.hrl").

%% API
-export([t/0, t0/2, t1/3, t2/3]).

t() ->
  erlang:spawn(fun() ->
    clicks(estats_offer_server:pid(), 100000)
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
  timer:sleep(5),
  clicks(Pid, N-1).

random_click() ->
  Date = { 2013, 2, 10 }, % { 2013, random:uniform(2), random:uniform(28) },
  #click_info {
    id = random:uniform(10000),
    offer_id = random:uniform(2),
    offer_url_id = random:uniform(4),
  % banner id (не реализовано)
    affiliate_id = random:uniform(2),
    advertiser_id = random:uniform(2),
    subid = dict:from_list([
      { 1, random_prop({<<"1">>, <<"2">>, <<"3">>, <<"4">>}) },
      { 2, random_prop({<<"1">>, <<"2">>}) },
      { 3, random_prop({<<"1">>, <<"2">>}) },
      { 4, random_prop({<<"1">>, <<"2">>}) },
      { 5, random:uniform(10000) }
    ]),  % Список subid
    http_referer = <<"http://referer.com/hello/world?hi=123">>,
    domain = <<"referer.com">>,
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
  }.

random_prop(Tuple) ->
  element(random:uniform(tuple_size(Tuple)), Tuple).


