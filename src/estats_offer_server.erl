%% Copyright
-module(estats_offer_server).

-export([click/2, start_link/0, state/1]).

-include("include/click_info.hrl").
-include("include/offer_info.hrl").

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3,terminate/2]).

-record(state, {
  data :: offer_info
}).

-spec init({}) -> {ok, state}.
init({}) -> {ok, #state{
  data = estats_offer:new()
}}.

-spec click(Pid :: pid(), Click :: click_info) -> ok.
click(Pid, Click) ->
  gen_server:call(Pid, { click, Click }).

-spec state(Pid :: pid()) -> offer_info.
state(Pid) ->
  gen_server:call(Pid, state).

-spec start_link() -> {ok, pid() }.
start_link() ->
  gen_server:start_link(?MODULE, {}, []).

%% Обработка клика
-spec handle_cast({click, Click :: click_info}, State :: state) -> {noreply, state}.
handle_cast({click, Click}, State ) ->
  Affiliate_offer = { Click#click_info.offer_id, Click#click_info.affiliate_id },
  Advertiser_offer = { Click#click_info.offer_id, 0 },
  Date = { Click#click_info.year, Click#click_info.month, Click#click_info.day },
  Hour = Click#click_info.hour,
  Data = State#state.data,

  estats_report:counter_inc(Data#offer_info.count, Affiliate_offer , [ Date , Hour ]),
  estats_report:counter_inc(Data#offer_info.count, Advertiser_offer , [ Date , Hour ]),

  estats_report:counter_inc(Data#offer_info.count, Click#click_info.offer_id, [ Click#click_info.affiliate_id ]),

  dict:map(fun(Index, Subid_item) ->
    Subid_hash = estats_report:map_save(Data#offer_info.subid, Affiliate_offer, {Index, Subid_item} ),
    estats_report:counter_inc(Data#offer_info.subid, Affiliate_offer, [ Date, {Index, Subid_hash} , Hour ]),
    Subid_hash
  end, Click#click_info.subid),

  estats_report:counter_inc(Data#offer_info.domain, Affiliate_offer , [ Date, Click#click_info.domain ]),
  estats_report:counter_inc(Data#offer_info.domain, Advertiser_offer, [ Date, Click#click_info.domain ]),

  Referer_hash = estats_report:map_save(Data#offer_info.referer, Click#click_info.affiliate_id, Click#click_info.http_referer ),
  estats_report:counter_inc(Data#offer_info.referer, Advertiser_offer, [ Referer_hash, Date ]),

  {noreply, State }.

handle_call({click, Click}, _From, State) ->
  {noreply, NewState} = handle_cast({click, Click}, State),
  {reply, ok, NewState};

handle_call( state, _From, State ) -> { reply, State#state.data, State };

handle_call(_,_, State ) -> { noreply, State }.

handle_info(_, State) -> { noreply, State }.

code_change(_, State, _) -> { ok, State}.

terminate(_, _) -> ok.


%% -spec parse_sql_date(Date) -> { Year, Month, Day, Hour, Minutes, Sec } | false.
%% parse_sql_date(Date) ->
%%   try
%%     <<Year:4/binary-unit:8,
%%       _:1/binary-unit:8,
%%       Month:2/binary-unit:8,
%%       _:1/binary-unit:8,
%%       Day:2/binary-unit:8,
%%       _:1/binary-unit:8,
%%       Hour:2/binary-unit:8,
%%       _:1/binary-unit:8,
%%       Minutes:2/binary-unit:8,
%%       _:1/binary-unit:8,
%%       Sec:2/binary-unit:8,
%%       _/binary>> = Date of
%%       _ -> { Year, Month, Day, Hour, Minutes, Sec }
%%   catch
%%       _ -> false
%% end.

