-module(estats_subreport_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_child/3, ensure_no_child/0, exit/0]).

%% supervisor
-export([init/1]).

%% API
start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child(Type, Path, Mode) ->
    supervisor:start_child(?MODULE, [ Type, Path, Mode ]).

ensure_no_child() ->
  case length(supervisor:which_children(?MODULE)) == 0 of
    true -> ok;
    _ ->
      timer:sleep(100),
      ensure_no_child()
  end.

exit() ->
  lists:map(fun({ _, Pid, _, _} ) ->
    supervisor:terminate_child(?MODULE, Pid)
  end, supervisor:which_children(?MODULE)),
  ok.

%% supervisor callbacks
init([]) ->
  {ok, {{simple_one_for_one, 5, 10}, [
    { none, { estats_subreport_server, start_link, [] }, temporary, 600 * 1000 , worker, [] }
  ]}}.

