%% Copyright
-module(estats_sub_server).

-include("include/click_info.hrl").

-behaviour(gen_server).

%% API
-export([start_link/2]).

%% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

%% API
start_link(Redis, Offer_server) ->
  gen_server:start_link(?MODULE, {Redis, Offer_server}, []).

%% gen_server callbacks
-record(state, {
  redis_pid :: pid(),
  offer_pid :: pid(),
  queue :: string(),
  queue_b :: binary()
}).

init({{redis, Redis}, Offer_server}) ->
  {ok, Sub} = { ok, none }, %% eredis:start_link(Redis),
  {ok, Offer } = estats_offer_server:start_link(Offer_server),
  {queue, Queue } = proplists:lookup(queue, Redis),
  %tick_init(), Временно не принимаем данные
  {ok, #state{
    redis_pid = Sub,
    offer_pid = Offer,
    queue = Queue,
    queue_b = list_to_binary(Queue)
  }}.

handle_call(_Request, _From, State) ->
  {noreply, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(tick, State) ->
  Ans = eredis:q(State#state.redis_pid, ["BLPOP", State#state.queue, 1 ]),
  case Ans of
    { error, no_connection } ->
      tick_init(),
      { noreply, State };
    {ok, undefined } ->
      tick_init(),
      { noreply, State };
    {ok, [ Name, Json ] } when Name =:= State#state.queue_b ->
      Proplist = jsx:decode(Json),

      Year = from_binary(proplists:get_value(<<"date_year">>, Proplist, undefined)),
      Month = from_binary(proplists:get_value(<<"date_month">>, Proplist, undefined)),
      Day = from_binary(proplists:get_value(<<"date_day">>, Proplist, undefined)),

      Date = {Year, Month, Day},

      Subid = lists:map(fun(Number) ->
        { Number, from_binary(proplists:get_value(<< <<"aff_subid">>/binary, (integer_to_binary(Number))/binary >>, Proplist, <<"">>)) }
      end, lists:seq(1,5)),
      case date:is_valid(Date) of
        false -> error;
        true ->
          Click = #click_info{
            id = from_binary(proplists:get_value(<<"id">>, Proplist, undefined)),
            offer_id = from_binary(proplists:get_value(<<"offer_id">>, Proplist, undefined)),
            offer_url_id = from_binary(proplists:get_value(<<"offer_id">>, Proplist, undefined)),
            advertiser_id = from_binary(proplists:get_value(<<"advertiser_id">>, Proplist, undefined)),
            affiliate_id = from_binary(proplists:get_value(<<"affiliate_id">>, Proplist, undefined)),
            hour = from_binary(proplists:get_value(<<"date_hour">>, Proplist, undefined)),
            year = Year,
            month = Month,
            day = Day,
            date = Date,
            day_number = calendar:date_to_gregorian_days(Date),
            day_of_week = calendar:day_of_the_week(Date),
            is_unique = from_binary(proplists:get_value(<<"is_unique">>, Proplist, undefined)) > 0,
            ip = proplists:get_value(<<"ip">>, Proplist, undefined),
            http_referer = proplists:get_value(<<"referer">>, Proplist, undefined),
            domain = proplists:get_value(<<"referer_host">>, Proplist, undefined),
            user_agent = proplists:get_value(<<"useragent">>, Proplist, undefined),
            subid = dict:from_list([ { Index, Item } || { Index, Item} <- Subid, Item =/= ''])

          },
          error_logger:info_report({Json, Click}),
          estats_offer_server:click(State#state.offer_pid, Click)
      end,
      handle_info(tick, State)
  end;

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

tick_init() ->
  {ok, _Ref } = timer:send_after(1000, tick),
  ok.

from_binary(Term) when not(is_binary(Term)) -> Term;
from_binary(Term) ->
  try
    binary_to_integer(Term)
  catch
    error:badarg ->
      try
        binary_to_existing_atom(Term, utf8)
      catch
        error:badarg ->
          Term
      end
  end.

