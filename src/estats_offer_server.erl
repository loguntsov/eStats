%% Copyright
-module(estats_offer_server).

-export([click/2, start_link/0, state/1, report/5, pid/0]).

-include("include/click_info.hrl").
-include("include/offer_info.hrl").

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3,terminate/2]).

-export([send_report_query/4]).

-record(state, {
  reports :: dict(), % {Report_module(), pid()
  is_readonly :: boolean()
}).

init({}) ->
  gproc:add_local_name(offer_server),
  {ok, #state{
  reports = dict:from_list(lists:map(
    fun(Report_module) ->
      { ok, Pid } = estats_gen_report:start_link(Report_module),
      {Report_module, Pid}
    end, [ estats_report_count ])),
  is_readonly = false
}};

init({readonly}) -> {ok, #state{
  reports = dict:new(),
  is_readonly = true
} }.

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

-spec start_link() -> {ok, pid() }.
start_link() ->
  gen_server:start_link(?MODULE, {}, []).

%% Обработка клика
handle_call({click, Click}, _From, State) ->
  case State#state.is_readonly of
    false ->
      dict:map(fun(_Report_module, Pid) ->
        ok = estats_gen_report:click(Pid, Click)
      end, State#state.reports),
      {reply, ok, State};
    true -> { reply, {error, readonly}, State }
  end;

handle_call( state, _From, State ) -> { reply, State#state.reports, State };
handle_call(_,_, State ) -> { noreply, State }.

%% Выдача отчета
handle_cast({report, From, Ref, {Report_module, Query } }, State) ->
  case dict:find(Report_module, State#state.reports) of
    error -> From!{report_error, Ref, report_module_not_found};
    {ok, ReportPid } ->
      spawn_link(?MODULE, send_report_query, [ From, Ref, ReportPid, Query ])
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
      {error, Reason } -> From!{report_error, Ref, Reason}
    end
  catch
    error:Reason_error -> From!{report_error, Ref, Reason_error}
  end.

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

