%% Copyright
-module(biznip_import).

-include("../include/logger.hrl").

%% API
-export([execute/1, get_clicks/2, get_total_count/0, send_clicks/2]).


execute(From) ->
  application:start(inets),
  Number = get_total_count(),
  ?INFO_(Number),
  send_clicks(From, Number),
  ok.

send_clicks(From, To) when To =< From ->
  estats_subreport_sup:exit(),
  estats_subreport_sup:ensure_no_child(),
  ?INFO_("Import is done"),
  ok;

send_clicks(From, To) ->
  Count = min(To - From, 1000),
  Clicks = get_clicks(To - Count, To),
  lists:foldl(fun(Click, _) ->
    estats_offer_server:click(estats_offer_server:pid(), Click),
    ok
  end, ok, Clicks),
  send_clicks(From, (To-Count) -1).


get_clicks(From, To) when From < 0 ->
  get_clicks(0, To);

get_clicks(From, To) when To < From -> [];

get_clicks(From, To) ->
  ?INFO_({From, To}),
  {ok, Response} = httpc:request("http://my.biznip.com/api/biznip/export_clicks?from=" ++ integer_to_list(From) ++ "&to=" ++ integer_to_list(To)),
  Json = element(3, Response),
  [ _ | List ] = lists:foldl(fun
    ( 0, [ String | StringList ]) ->
      [ [] , String | StringList ];
    ( Char, [ String | StringList ]) ->
      [ [ Char | String ] | StringList ]
  end, [[]], Json),
  lists:map(fun(String) ->
    {ok, Click_info } = estats_click:from_json(list_to_binary(lists:reverse(String))),
    Click_info
  end, List).

get_total_count() ->
  {ok, Response} = httpc:request("http://my.biznip.com/api/biznip/export_clicks/total"),
  list_to_integer(element(3, Response)).