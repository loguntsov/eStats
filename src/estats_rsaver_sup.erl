-module(estats_rsaver_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, sync_task/2, is_tasks_done/1]).

%% supervisor
-export([init/1]).

%% API
start_link() ->
  supervisor:start_link(?MODULE, []).

sync_task(PidSup, Msg) ->
  {ok, ChildPid } = supervisor:start_child(PidSup,[]),
  estats_rsaver:send_sync(ChildPid, Msg),
  {ok, ChildPid }.

is_tasks_done(PidSup) ->
  length(supervisor:which_children(PidSup)) == 0.

%% supervisor callbacks
init([]) ->
  {ok, {{simple_one_for_one, 0, 10}, [
    { none, { estats_rsaver, start_link, [ ] }, temporary, 60000, worker, [] }
  ]}}.
