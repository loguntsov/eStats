%% Copyright
-module(estats_gen_report).

-include("include/report_info.hrl").
-include("include/click_info.hrl").

%% API

-export([start_link/3, click/2, report/2]).

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

-record(state,{
  report_module :: atom(),
  mode :: atom(),
  path :: string(),
  report_mode :: []
}).

-define(REPORT_MAX_SIZE, 536870912). % Максимальный размер файлов отчета за период (512 мб)
-define(REPORT_MAX_DATE_LENGHT, 30). % Максимальное кол-во дней в одном файле отчете (в периоде)

-callback handle_click(Click :: click_info, Report :: report_info) ->
  ok.

-callback handle_report(Query :: term(), Report :: report_info) ->
  { ok, Group :: integer(), [ { Key :: list(), Value :: integer() } ] } |
  { error, Reason :: term() }.

-callback handle_info(Type :: atom()) -> { Property_list :: [ atom() ], Group_list :: [ atom() ] }.

init({Module, Path, Mode}) ->
  %process_flag(trap_exit, true),
  gproc:add_local_name({Module, Mode}),
  {ok, #state{
    report_module = Module,
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
  { message_queue_len, Msgs } = process_info(Pid, message_queue_len ),
  if
    Msgs > 10 ->
      timer:sleep(1),
      click(Pid, Click);
    true ->
      gen_server:cast(Pid, {click, Click}),
      ok
  end.

-spec report(Pid :: pid(), Query :: any()) -> Reply :: any().
report(Pid, Query) ->
  gen_server:call(Pid, { report, Query}).

handle_cast({click, Click}, State) when State#state.mode =/= readonly ->
  Pid = get_report_pid(Click#click_info.date, State),
  ok = estats_subreport_server:click(Pid, Click),
  {noreply, State};

handle_cast(_, State) -> { noreply, State }.

handle_call({report, {Type, Period, Query } }, _From, State) ->

  SubReports =
    lists:foldl(fun(Date, Dict) ->
      Pid = get_report_pid(Date, State),
      dict:append(Pid, Date, Dict)
    end, dict:new() ,Period),

%% Последовательное выполнение запросов
%%     Result = lists:map(fun({Report, Dates}) ->
%%       try
%%         (NewState#state.report_module):handle_report({Type, Dates, Query } , Report)
%%       catch
%%         error:function_clause -> { error, report_unknown }
%%       end
%%     end, dict:to_list(Reports)),

%% Параллельное выполнение запросов
    {Collector, _ } = lists:mapfoldl(fun({Pid, Dates}, Acc) ->
      { { Acc, { Pid, {Type, Dates, Query } } }, Acc+1 }
    end,0,dict:to_list(SubReports)),

    Result = code_collector:start(fun({Pid, QueryAll}) ->
      estats_subreport_server:report(Pid, QueryAll)
    end, Collector),

    R = case Result of
      {ok, Ret } ->
        {ok, lists:concat(
              lists:map(
                fun({ _, {ok, Local_result} }) -> Local_result end,
              Ret )
             )
        };
      {error, Reason } -> { error, Reason }
    end,
    {reply, R , State};

handle_call( state, _From, State) ->
  {reply, State , State}.

handle_info(_, State) -> { noreply, State }.

code_change(_, State, _) -> { ok, State}.

terminate(_, _State) ->
  ok.

-spec get_report_pid(Date :: date(), State :: state) -> pid() | undefined.
get_report_pid(Date, #state{ report_module=Type, path=MainPath, mode=Mode } = _State) ->
  case estats_subreport_server:get_pid_by_date(Type, Date) of
    Pid when is_pid(Pid) -> Pid;
    undefined ->
      case get_report_path(MainPath, Type, Date, Mode) of
        { error, _ } -> undefined;
        { ok, Path } ->
          Pid0 = case estats_subreport_server:get_pid_by_path(Type, Path) of
            undefined ->
              estats_subreport_server:create(Type, Path, Mode);
            Pid when is_pid(Pid) -> Pid
          end,
          ok = estats_subreport_server:register(Pid0, Date),
          Pid0
      end
  end.

get_report_path(MainPath, Type, Date, Mode) ->
  Key = get_storage_key(Type, Date),
  case estats_storage:get(Key) of
    undefined ->
      case find_prev_report_path(Type, Date) of
        { ok, Path } ->
          case  estats_report:files_size(Path) of
            {ok, Size } ->
              if
                Size < ?REPORT_MAX_SIZE -> {ok, Path};
                true -> make_report_path(MainPath, Type, Date, Mode)
              end;
            _ -> make_report_path(MainPath, Type, Date, Mode)
          end;
        { error, not_found } ->
          make_report_path(MainPath, Type, Date, Mode)
      end;
    Path when is_list(Path) -> {ok, Path }
  end.

make_report_path(_Path, _Type, _Date, Mode ) when Mode =:= readonly -> { error, readonly };
make_report_path(Path, Type, Date, _ ) ->
  P = lists:concat([Path, "/", atom_to_list(Type), "/", date:to_string(Date,"/")]),
  ok = make_report_dir(P),
  Key = get_storage_key(Type, Date),
  estats_storage:put(Key, P),
  { ok, P }.

find_prev_report_path(Type, Date) ->
  find_prev_report_path(Type, Date, 30).

find_prev_report_path(_Type, _Date, 0) -> { error, not_found };
find_prev_report_path(Type, Date, Number) ->
  Key = get_storage_key(Type, Date),
  case estats_storage:get(Key) of
    undefined -> find_prev_report_path(Type, date:next_days(Date, -1), Number - 1);
    Path when is_list(Path) -> {ok, Path}
  end.


get_storage_key(Type, Date) ->
  {report, Type, Date}.

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
