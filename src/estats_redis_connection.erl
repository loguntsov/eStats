%% Copyright
-module(estats_redis_connection).

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

%% API
start_link(Connection) ->
  gen_server:start_link(?MODULE, Connection, []).

%% gen_server callbacks
-record(state, {
  redis_pid :: pid(),
  queue :: string(),
  queue_b :: binary()
}).

init(Connection) ->
  {ok, Sub} = eredis:start_link(Connection),
  {queue, Queue } = proplists:lookup(queue, Connection),
  tick_init(),
  {ok, #state{
    redis_pid = Sub,
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
      case estats_click:from_json(Json) of
        {ok, Click} ->
          estats_offer_server:click(estats_offer_server:pid(), Click);
        _ -> no_ok
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
