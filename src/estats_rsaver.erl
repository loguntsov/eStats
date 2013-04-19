%% Copyright
-module(estats_rsaver).

-include("include/logger.hrl").
-include("include/report_info.hrl").

%% API
-export([flush/2, start_link/0, send_sync/2]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

-record(state, { }).


start_link() ->
  gen_server:start_link(?MODULE, {}, []).

-spec flush(PidSup :: pid(), Report :: report_info ) -> {ok, report_info }.
flush(PidSup, Report) ->
  case estats_report:is_ets_empty(Report) of
    undefined -> { ok, Report };
    _ ->
      estats_rsaver_sup:sync_task(PidSup, { ets, counter, Report#report_info.module, Report#report_info.counters, Report#report_info.counters_data }),
      estats_rsaver_sup:sync_task(PidSup, { ets, index, Report#report_info.module, Report#report_info.index, Report#report_info.index_data }),
      estats_rsaver_sup:sync_task(PidSup, { ets, map, Report#report_info.module, Report#report_info.map, Report#report_info.map_data }),
      { ok, Report#report_info{
        counters = undefined,
        map = undefined,
        index = undefined
      }}
  end.

init(_Options) ->
  { ok, #state{} }.

handle_cast({ sync, Module, { counter, Ets, Dets } }, State) ->
  ok = ets:foldl(fun(Term, _) ->
    Key = element(1,Term),
    case estats_table:lookup(dets, Dets, Key) of
      [ ] ->
        dets:insert(Dets, Term);
      [ Tuple ] when is_tuple(Tuple) ->
        dets:insert(Dets, list_to_tuple(
          [ Key | estats_counter:step_sum(estats_counter:tuple_to_value_list(Tuple), estats_counter:tuple_to_value_list(Term) )]
        ))
    end,
    run_from_report(Module, handle_mysql_counter, Term),
    ok
  end, ok, Ets),
  ets:delete(Ets),
  dets:sync(Dets),
  { stop, normal, State };

handle_cast({sync, _Module, { index, Ets, Dets }}, State) ->
  ok = ets:foldl(fun({Key, Value}, _) ->
    dets:insert(Dets, { Key, Value })
  end, ok, Ets),
  ets:delete(Ets),
  dets:sync(Dets),
  { stop, normal, State };

handle_cast({sync, _Module, { map, Ets, Dets }}, State) ->
  ok = ets:foldl(fun({Key, Value}, _) ->
    dets:insert(Dets, { Key, Value })
  end, ok, Ets),
  ets:delete(Ets),
  dets:sync(Dets),
  { stop, normal, State };

handle_cast(Msg, State) -> { stop, Msg, State }.

handle_call(Msg, _, State) -> { stop, Msg, State }.

handle_info({'ETS-TRANSFER', _,_,_}, State) -> { noreply, State };

handle_info(Msg, State) -> { stop, Msg, State }.

code_change(_, State, _) -> { ok, State }.

terminate(_, _State) -> ok.

send_sync(Pid, { ets, Type, Module, Ets, Dets} ) ->
  ets:give_away(Ets, Pid, {}),
  gen_server:cast(Pid, { sync, Module, { Type, Ets, Dets } }).

run_from_report(Module, FuncName, Term) ->
  ?INFO_({ run_from_report ,  Module, FuncName, Term }),
  try
    ok = Module:FuncName(Term),
    ok
  catch
    error:function_clause -> ok
  end.