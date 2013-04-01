%% Copyright
-module(estats_rsaver).

-include("include/report_info.hrl").

%% API
-export([flush/1, close/1, start_link/0]).

-behaviour(gen_server).

-record(state, { }).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).


start_link() ->
  gen_server:start_link(?MODULE, {}, []).

-spec flush(Report :: report_info ) -> {ok, report_info} | { error, Reason :: term() }.
flush(Report) when Report#report_info.counters =:= none, Report#report_info.map =:= none, Report#report_info.index =:= none -> { ok, Report };
flush(Report) ->
  give_ets(Report),
  gen_server:cast(pid(), { sync, Report }),
  { ok, Report#report_info{
    counters = none,
    map = none,
    index = none
  }}.

close(Report) ->
  give_ets(Report),
  gen_server:cast(pid(), { close, Report }),
  ok.

init(_Options) ->
  gproc:add_local_name(rsaver),
  { ok, #state{} }.

handle_cast({ sync, Report }, State) ->
  ok = ets:foldl(fun(Term, _) ->
    Key = element(1,Term),
    case dets:lookup(Report#report_info.counters_data, Key) of
      [ ] ->
        dets:insert(Report#report_info.counters_data, Term);
      [ Tuple ] when is_tuple(Tuple) ->
        dets:insert(Report#report_info.counters_data, list_to_tuple(
          [ Key | estats_counter:step_sum(estats_counter:tuple_to_value_list(Tuple), estats_counter:tuple_to_value_list(Term) )]
        ))
    end,
    ok
  end, ok, Report#report_info.counters),
  dets:sync(Report#report_info.counters_data),
  ets:delete(Report#report_info.counters),

  ok = ets:foldl(fun({Key, Value}, _) ->
    dets:insert(Report#report_info.index_data, { Key, Value })
  end, ok, Report#report_info.index),
  dets:sync(Report#report_info.index_data),
  ets:delete(Report#report_info.index),

  ok = ets:foldl(fun({Key, Value}, _) ->
    dets:insert(Report#report_info.map_data, { Key, Value })
  end, ok, Report#report_info.map),
  dets:sync(Report#report_info.map_data),
  ets:delete(Report#report_info.map),

  { noreply, State };

handle_cast({close, Report}, State) ->
  { noreply , NewState } = handle_cast({sync, Report}, State),
  dets:close(Report#report_info.counters_data),
  dets:close(Report#report_info.index_data),
  dets:close(Report#report_info.map_data),
  { noreply, NewState };

handle_cast( _, State) -> { noreply, State }.

handle_call(_, _, State) -> { noreply, State }.

handle_info(_, State) -> { noreply, State }.

code_change(_, State, _) -> { ok, State }.

terminate(_, _State) -> ok.

pid() ->
  gproc:lookup_local_name(rsaver).


give_ets(Report) ->
  ets:give_away(Report#report_info.counters, pid(), {}),
  ets:give_away(Report#report_info.map, pid(), {}),
  ets:give_away(Report#report_info.index, pid(), {}),
  ok.