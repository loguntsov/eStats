%% Copyright
-module(estats_report_sup).
-author("begemot").

-behaviour(supervisor).

%% API
-export([start_link/3, pids/1, pid_module/2,list/1]).

%% supervisor
-export([init/1]).

%% API
start_link(Report_modules, Path, Mode) ->
  supervisor:start_link(?MODULE, {Report_modules, Path, Mode}).

%% supervisor callbacks
init({Reports_modules, Path, Mode}) ->
  {ok, {{one_for_one, 1, 10}, [
    { Module , { estats_gen_report, start_link, [ Module, Path, Mode ] }, permanent, 100000, worker  , dynamic } || Module <-Reports_modules ]
  }}.

-spec list(Pid :: pid()) -> [ { Module :: atom(), Pid :: pid() } ].
list(Pid) ->
  [ { Id, Pid_child } || {Id, Pid_child, _Type, _Modules} <- supervisor:which_children(Pid) ].

-spec pids(Pid :: pid()) -> [ Pid :: pid() ].
pids(Pid) ->
  [ Pid_child || {_Id, Pid_child, _Type, _Modules} <- supervisor:which_children(Pid), is_pid(Pid_child) ].

-spec pid_module(Pid :: pid(), Module :: atom()) -> { ok, Pid :: pid() } | {error, undefined}.
pid_module(Pid, Module) ->
  Ret = [ Pid_child || { M, Pid_child, _Type, _Modules} <- supervisor:which_children(Pid), M =:= Module, is_pid(Pid_child) ],
  case Ret of
    [ P ] -> {ok, P };
    [ ] -> { error, undefined }
  end.

