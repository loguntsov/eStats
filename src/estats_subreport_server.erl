-module(estats_subreport_server).

-behaviour(gen_server).

-include("include/types.hrl").
-include("include/click_info.hrl").
-include("include/report_info.hrl").
-include("include/logger.hrl").

%% API
-export([start_link/3, create/3, get_pid_by_date/2, get_pid_by_path/2,click/2, report/2, register/2]).

%% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-define(USE_TIMEOUT, 5). %% Время бездействия, до выключения данного субсервера, в минутах
-define(SAVE_TIMEOUT, 6000). %% Время между двумя попытками записать данные на диск, мс.

%% API
start_link(Type, Path, Mode) ->
  gen_server:start_link(?MODULE, {Type, Path, Mode}, []).

%% gen_server callbacks
-record(state, {
  type :: atom(),
  report :: report_info,
  save_tick :: boolean(),
  is_changed :: boolean(),
  rsaver_sup :: pid(),
  last_use :: timestamp(),
  mode :: readonly | readwrite
}).

init({Type, Path, Mode }) ->
  process_flag(trap_exit, true),
  gproc:add_local_name({report_path, Type, Path}),
  {ok, SupPid } = estats_rsaver_sup:start_link(),
  {ok , Report } = estats_report:open(Type, Path, [{ mode, Mode}]),
  tick(),
  {ok, #state{
    save_tick = false,
    mode = Mode,
    report = Report,
    type = Type,
    rsaver_sup = SupPid
  }}.

-spec create(Type :: atom(), Path :: list(), Mode :: atom()) -> pid().
create(Type, Path, Mode) ->
  {ok, Pid } = estats_subreport_sup:start_child(Type, Path, Mode),
  Pid.


-spec register(Pid :: pid(), Date :: date()) -> ok.
register(Pid, Date) ->
  gen_server:call(Pid, { register, Date }).

-spec get_pid_by_date(Type :: atom(), Date :: date()) -> pid() | undefined.
get_pid_by_date(Type, Date) when is_tuple(Date) ->
  gproc:lookup_local_name({report_date, Type, Date}).

get_pid_by_path(Type, Path) when is_list(Path) ->
  gproc:lookup_local_name({report_path, Type, Path}).

-spec click(Pid :: pid(), Click :: click_info) -> ok | { error, Reason :: term() }.
click(Pid, Click) ->
  { message_queue_len, Msgs } = process_info(Pid, message_queue_len ),
  if
    Msgs > 10 ->
      timer:sleep(1),
      click(Pid, Click);
    true ->
      gen_server:cast(Pid, { click, Click}),
      ok
  end.

-spec report(Pid :: pid(), Query :: tuple() )-> { ok, Result :: term() } | { error, Reason :: term() }.
report(Pid, Query) ->
  gen_server:call(Pid, { report, Query }).

handle_call({register, Date }, _From, State) ->
  gproc:add_local_name({report_date, State#state.report#report_info.module, Date}),
  { reply, ok, State };

handle_call({report, QueryAll}, _From, State) ->
  Result = try
    (State#state.report#report_info.module):handle_report(QueryAll, State#state.report)
  catch
    error:function_clause -> { error, report_unknown, QueryAll }
  end,
  { reply, Result, now_use(State)};

handle_call(_Request, _From, State) ->
  {noreply, State}.

handle_cast({click, Click }, #state { report = Report } = State) when State#state.mode =/= readonly ->
  NewState = case estats_report:is_ets_empty(Report) of
    undefined -> State#state{ report = estats_report:create_ets(Report) };
    _ ->
      State
  end,
  estats_report:index_add(NewState#state.report, dates_info, Click#click_info.date ),
  (NewState#state.report#report_info.module):handle_click(Click, NewState#state.report),
  {noreply, now_use(now_chanded(NewState))};

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(tick, State) when not(State#state.is_changed) ->
  Time = timer:now_diff(os:timestamp(), State#state.last_use),
  if
    Time > ?USE_TIMEOUT * 60000000 ->
      { stop, normal, State };
    true ->
      tick(),
      { noreply, State }
  end;

handle_info(tick, State) ->
  case estats_rsaver_sup:is_tasks_done(State#state.rsaver_sup) of
    true ->
      {ok, Report } = estats_rsaver:flush(State#state.rsaver_sup,State#state.report),
      tick(),
      {noreply, State#state{
        report = estats_report:create_ets(Report),
        is_changed = false
      }};
    false ->
      tick(),
      { noreply, State }
  end;

handle_info( info, State) ->
  ?INFO_(State),
  { noreply, State };

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, State) ->
  {ok, NewReport } = estats_rsaver:flush(State#state.rsaver_sup,State#state.report),
  ensure_task_done(State#state.rsaver_sup),
  estats_report:close(NewReport),
  ok.

ensure_task_done(RsaverSupPid) ->
  case estats_rsaver_sup:is_tasks_done(RsaverSupPid) of
    true -> ok;
    _ ->
      timer:sleep(1),
      ensure_task_done(RsaverSupPid)
  end.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

-spec tick() -> ok.
tick() ->
  timer:send_after(?SAVE_TIMEOUT, tick ),
  ok.

now_use(State) ->
  State#state { last_use = os:timestamp() }.

now_chanded(State) ->
  State#state { is_changed = true }.

