%% Copyright
-module(t).

-include("include/click_info.hrl").
-include("include/offer_info.hrl").

%% API
-export([t/0, t0/1, t1/2,t2/2]).

t() ->
  clicks(estats_offer_server:pid(), 10000),

  Pid = gproc:lookup_local_name(estats_report_count),
  gen_server:call(Pid, state).

t0(Query) ->
  estats_offer_server:report(estats_offer_server:pid(), estats_report_count, affiliates_of_offer, [{2013,2,20}] , Query).

t1(Offer, Affiliate) ->
  estats_offer_server:report(estats_offer_server:pid(), estats_report_count, affiliates_hour_count, {2013,2,20} , {Offer, Affiliate}).

t2(Offer, Affiliate) ->
  estats_offer_server:report(estats_offer_server:pid(), estats_report_count, affiliates_day_count, {2013,2,20} , {Offer, Affiliate}).


clicks(_Pid, 0) -> ok;

clicks(Pid, N) ->
  estats_offer_server:click(Pid, random_click()),
  clicks(Pid, N-1).

random_click() ->
  Date = { 2013, 2, 20 },
  #click_info {
  id = random:uniform(10000),
  transaction_id = <<"">>,
  offer_id = random:uniform(30),
  offer_url_id = random:uniform(30),
% banner id (не реализовано)
  affiliate_id = random:uniform(10),
% advertiser_id :: integer(), % ID рекламодателя
  subid = dict:from_list([
    { 1, random_prop({<<"1">>, <<"2">>, <<"3">>, <<"4">>, <<"5">>, <<"6">>, <<"7">>, <<"8">>, <<"9">>, <<"10">>, <<"11">>}) },
    { 2, random_prop({<<"1">>, <<"2">>, <<"3">>, <<"4">>, <<"5">>, <<"6">>, <<"7">>, <<"8">>, <<"9">>, <<"10">>, <<"11">>}) },
    { 3, random_prop({<<"1">>, <<"2">>, <<"3">>, <<"4">>, <<"5">>, <<"6">>, <<"7">>, <<"8">>, <<"9">>, <<"10">>, <<"11">>}) },
    { 4, random_prop({<<"1">>, <<"2">>, <<"3">>, <<"4">>, <<"5">>, <<"6">>, <<"7">>, <<"8">>, <<"9">>, <<"10">>, <<"11">>}) },
    { 5, random_prop({<<"1">>, <<"2">>, <<"3">>, <<"4">>, <<"5">>, <<"6">>, <<"7">>, <<"8">>, <<"9">>, <<"10">>, <<"11">>}) }
  ]),  % Список subid
  http_referer = <<"http://referer.com/hello/world?hi=123">>,
  domain = <<"referer.com">>,
  user_agent = <<"Unknown">>,
  ip = <<"10.123.123.54">>,
  year = date:year(Date),
  month = date:month(Date), %random:uniform(2),
  day = date:day(Date), %random:uniform(30),
  day_number = calendar:date_to_gregorian_days(Date),
  hour = random:uniform(24) - 1,
  date = Date
}.

random_prop(Tuple) ->
  element(random:uniform(tuple_size(Tuple)), Tuple).


