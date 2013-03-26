%% Copyright
-module(estats_offer_server).

-export([click/2, start_link/1, state/1, report/5, pid/0]).

-include("include/click_info.hrl").
-include("include/offer_info.hrl").

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3,terminate/2]).

-export([send_report_query/4]).

-record(state, {
  reports_sup :: pid(),
  is_readonly :: boolean()
}).

init({Path}) ->
  gproc:add_local_name(offer_server),
  { ok , Pid } = estats_report_sup:start_link([
    estats_report_count,
    estats_report_subid,
    estats_report_total
  ], Path, write),
  {ok, #state{
    reports_sup = Pid,
    is_readonly = false
}}.


-spec pid() -> pid().
pid() ->
  gproc:lookup_local_name(offer_server).

-spec click(Pid :: pid(), Click :: click_info) -> ok | { error, Reason :: term()}.
click(Pid, Click) ->
  gen_server:call(Pid, { click, Click }).

-spec state(Pid :: pid()) -> offer_info.
state(Pid) ->
  gen_server:call(Pid, state).

-spec report(Pid :: pid(), Report_module :: atom(), Type :: atom(), Period :: date_period(), Query :: term()) -> {ok, Result :: term()} | {error, Reason :: term()}.
report(Pid, Report_module, Type, Period, Query) ->
  Ref = make_ref(),
  gen_server:cast(Pid, {report, self(), Ref, {Report_module, { Type, Period, Query} } }),
  receive % Сообщения отправляются из send_report_query
    {report_answer, Ref, Result} -> {ok, Result};
    {report_error, Ref, Reason } -> {error, Reason }
  after 10000 ->
    {error, timeout}
  end.

-spec start_link(Path :: string()) -> {ok, pid() }.
start_link(Path) ->
  gen_server:start_link(?MODULE, {Path}, []).

%% Обработка клика
handle_call({click, Click}, _From, State) ->
  case State#state.is_readonly of
    false ->
      lists:map(fun(Pid) ->
        ok = estats_gen_report:click(Pid, Click)
      end, estats_report_sup:pids(State#state.reports_sup)),
      {reply, ok, State};
    true -> { reply, {error, readonly}, State }
  end;

handle_call( state, _From, State ) -> { reply, estats_report_sup:list(State#state.reports_sup), State };
handle_call(_,_, State ) -> { noreply, State }.

%% Выдача отчета
handle_cast({report, From, Ref, {Report_module, Query } }, State) ->
  case estats_report_sup:pid_module(State#state.reports_sup, Report_module) of
    { ok, ReportPid } ->
        spawn_link(?MODULE, send_report_query, [ From, Ref, ReportPid, Query ]);
    { error, undefined } ->
        report_error(From, Ref, report_module_not_found)
  end,
  {noreply, State}.

send_report_query(From, Ref, ReportPid, {Type, Period, Query}) ->
  try
    Date_list = if
      is_tuple(Period) -> if
        tuple_size(Period) =:= 3 -> [ Period ];
        tuple_size(Period) =:= 2 -> date:period_to_list(Period)
      end;
      is_list(Period) -> Period
    end,
    case estats_gen_report:report(ReportPid, {Type, Date_list, Query}) of
      {ok, Result } -> From!{report_answer, Ref, Result};
      {error, Reason } -> report_error(From, Ref, Reason)
    end
  catch
    error:Reason_error -> report_error(From, Ref, Reason_error)
  end.

report_error(Pid, Ref, Reason) ->
  Pid!{report_error, Ref, Reason}.

handle_info(_, State) -> { noreply, State }.

code_change(_, State, _) -> { ok, State}.

terminate(_, _) -> ok.

