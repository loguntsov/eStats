%% Copyright
-module(estats_gen_report).

-include("include/report_info.hrl").
-include("include/click_info.hrl").

%% API

-export([start_link/3, click/2, report/2]).

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

-record(state,{
  reports :: dict(),
  dates :: dict(),
  report_module :: atom(),
  mode :: atom(),
  path :: string(),
  report_mode :: []
}).

-define(REPORT_MAX_SIZE, 536870912). % Максимальный размер файлов отчета за период (512 мб)
-define(REPORT_MAX_DATE_LENGHT, 7). % Максимальное кол-во дней в одном файле отчете (в периоде)

-callback handle_click(Click :: click_info, Report :: report_info) ->
  ok.

-callback handle_report(Query :: term(), Report :: report_info) ->
  { ok, Group :: integer(), [ { Key :: list(), Value :: integer() } ] } |
  { error, Reason :: term() }.

init({Module, Path, Mode}) ->
  %process_flag(trap_exit, true),
  gproc:add_local_name({Module, Mode}),
  {ok, #state{
    report_module = Module,
    reports = dict:new(),
    dates = dict:new(),
    mode = Mode,
    path = Path,
    report_mode = [
      { mode , Mode },
      { save_interval, 60 }
    ]
  }}.


-spec start_link(Module :: atom(),Path :: string(), Mode :: atom()) -> {ok, pid()}.
start_link(Module,Path,Mode) ->
  gen_server:start_link(?MODULE, {Module, Path, Mode}, []).

-spec click(Pid :: pid(), Click :: click_info) -> ok.
click(Pid, Click) ->
  gen_server:cast(Pid, {click, Click}).

-spec report(Pid :: pid(), Query :: any()) -> Reply :: any().
report(Pid, Query) ->
  gen_server:call(Pid, { report, Query}).

handle_cast({click, Click}, State) when State#state.mode =/= readonly ->
  { NewState, Report } = get_report(Click#click_info.date, State),
  estats_report:index_add(Report, dates_info, Click#click_info.date ),
  ok = (State#state.report_module):handle_click(Click, Report),
  {noreply, NewState}.

handle_call({report, {Type, Period, Query } }, _From, State_main) ->
    { NewState, Reports } =
      lists:foldl(fun(Date, { State, Reports }) ->
        { NewState, Report} = get_report(Date, State),
        case Report of
          R when is_record(R, report_info) ->
            { NewState, dict:append(R, Date, Reports) };
          _ -> { NewState, Reports }
        end
      end, { State_main, dict:new() },Period),

%% Последовательное выполнение запросов
%%     Result = lists:map(fun({Report, Dates}) ->
%%       try
%%         (NewState#state.report_module):handle_report({Type, Dates, Query } , Report)
%%       catch
%%         error:function_clause -> { error, report_unknown }
%%       end
%%     end, dict:to_list(Reports)),

%% Параллельное выполнение запросов
    {Collector, _ } = lists:mapfoldl(fun({Report, Dates}, Acc) ->
      { { Acc, { Report, NewState#state.report_module, {Type, Dates, Query } } }, Acc+1 }
    end,0,dict:to_list(Reports)),

    Result = code_collector:start(fun({Report, Module, QueryAll}) ->
      try
        Module:handle_report(QueryAll, Report)
      catch
        error:function_clause -> { error, report_unknown }
      end
    end, Collector),

    R = case Result of
      {ok, Ret } ->
        List = [ { Cols, Local_result } || { _ , { ok, Cols, Local_result } } <- Ret ],
        [ { Cols_settings, _ } | _ ] = List,

        List1 = lists:usort(lists:concat([ Item || { _, Item } <- List])),
        R1 = case Cols_settings of
          Group when is_integer(Group) ->
            estats_report:group(Group, List1)
        end,
        { ok, R1 };
      {error, Reason } -> { error, Reason }
    end,
    {reply, R , NewState};

handle_call( state, _From, State) ->
  {reply, State , State}.

handle_info(_, State) -> { noreply, State }.

code_change(_, State, _) -> { ok, State}.

terminate(_, State) ->
  dict:map(fun(_Index, Report) ->
    estats_report:close(Report)
  end, State#state.reports),
  ok.


get_report(Date, State) ->
  Report = case dict:find(Date, State#state.dates) of
    {ok,RefReport} when is_reference(RefReport) ->
      case dict:find(RefReport, State#state.reports) of
        { ok, R } -> R;
        error -> error
      end;
    {ok, Data} -> Data;
    error -> error
  end,
  case Report of
    error ->
      get_report_from_storage(Date, State);
    R0 -> {State, R0}
  end.

get_report_from_storage(Date, State) ->
  Key = get_storage_key(Date, State),
  Path = case estats_storage:get(Key) of
    undefined ->
      case find_prev_report(Date, State, ?REPORT_MAX_DATE_LENGHT ) of
        undefined ->
          make_report(Date, State);
        P -> { ok, P }
      end;
    P -> {ok, P}
  end,
  case Path of
    { error, readonly } ->
      { State#state{
          dates = dict:store(Date, undefined , State#state.dates)
        },
        undefined
      };
    { ok, Path1 } ->
      {ok, Report } = estats_report:open(Path1, State#state.report_mode),
      Ref = make_ref(),
      { State#state{
          dates = dict:store(Date, Ref, State#state.dates),
          reports = dict:store(Ref, Report, State#state.reports)
        },
        Report
      }
  end.

make_report(_Date, State) when State#state.mode =:= readonly -> { error, readonly };
make_report(Date, State) ->
  P = lists:concat([State#state.path, "/", atom_to_list(State#state.report_module), "/", date:to_string(Date,"/")]),
  ok = make_report_dir(P),
  Key = get_storage_key(Date, State),
  estats_storage:put(Key, P),
  { ok, P }.

find_prev_report(_Date, _State, 0) -> undefined;
find_prev_report(Date, State, Limit) ->
  NewDate = date:next_days(Date, -1),
  Key = get_storage_key(NewDate, State),
  Path = case estats_storage:get(Key) of
    undefined -> find_prev_report(NewDate, State, Limit -1);
    P -> P
  end,

  if
    is_list(Path) ->
      case estats_report:files_size(Path) of
        {ok , Size } ->
          if
              Size < ?REPORT_MAX_SIZE -> Path;
              true -> undefined
          end;
        _ -> undefined
      end;
    true -> undefined
  end.


get_storage_key(Date, State) ->
  {report, State#state.report_module, Date}.

make_report_dir([]) -> { error, enoent};
make_report_dir(Path) ->
  case file:make_dir(Path) of
    ok -> ok;
    {error,enoent} ->
      case make_report_dir(string:join(lists:reverse(tl(lists:reverse(string:tokens(Path,"/")))),"/")) of
        ok -> file:make_dir(Path);
        Error -> Error
      end
  end.

