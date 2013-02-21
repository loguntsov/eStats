%% Copyright
-module(t).

-include("include/click_info.hrl").
-include("include/offer_info.hrl").

%% API
-export([t/0, t0/3]).

t() ->
  { ok, Pid } = estats_offer_server:start_link(),
  clicks(Pid, 1000000),

  (estats_offer_server:state(Pid))#offer_info.count.

t0(Report, A, B) ->
  estats_report:index_get_all(Report, A, B).

clicks(_Pid, 0) -> ok;

clicks(Pid, N) ->
  estats_offer_server:click(Pid, random_click()),
  clicks(Pid, N-1).

random_click() -> #click_info {
  id = random:uniform(10000),
  transaction_id = <<"">>,
  offer_id = random:uniform(3),
  offer_url_id = random:uniform(30),
% banner id (не реализовано)
  affiliate_id = random:uniform(30),
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
  year = 2013,
  month = 2,%random:uniform(2),
  day = 20,%random:uniform(30),
  hour = random:uniform(24)
}.

random_prop(Tuple) ->
  element(random:uniform(tuple_size(Tuple)), Tuple).

