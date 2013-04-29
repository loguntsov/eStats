%% Copyright
-module(biznip_sync_sup).
-author("begemot").

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% supervisor
-export([init/1]).

%% API
start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% supervisor callbacks
init([]) ->
  {ok, {{one_for_one, 5, 10}, [
    timed_supervisor_spec(click_sync, [ { biznip_clicks_sync, start_link, [] } , [
      { shedule, [ any,  [{"01:30:00", "6:00:00"}] ] }
    ]]),
    hashes_sync(hashes_1, [ { day, <<"hashes_1">> } ], [ any,  [{"01:30:00", "6:00:00"}] ]),
    hashes_sync(hashes_2, [ { day, <<"hashes_2">> }, <<"clicks > 1">> ], [ any,  [{"01:30:00", "6:00:00"}] ]),
    hashes_sync(hashes_3, [ { day, <<"hashes_3">> }, <<"clicks > 1">> ], [ any,  [{"01:30:00", "6:00:00"}] ]),
    hashes_sync(hashes_4, [ { day, <<"hashes_4">> }, <<"clicks > 1">> ], [ any,  [{"01:30:00", "6:00:00"}] ]),
    hashes_sync(hashes_5, [ { day, <<"hashes_5">> }, <<"clicks > 1">> ], [ any,  [{"01:30:00", "6:00:00"}] ]),
    hashes_sync(hashes_6, [ { day, <<"hashes_6">> }, <<"clicks > 1">> ], [ any,  [{"01:30:00", "6:00:00"}] ]),
    hashes_sync(hashes_7, [ { day, <<"hashes_7">> }, <<"clicks > 1">> ], [ any,  [{"01:30:00", "6:00:00"}] ]),
    hashes_sync(hashes_8, [ { day, <<"hashes_8">> }, <<"clicks > 1">> ], [ any,  [{"01:30:00", "6:00:00"}] ])
  ]}}.

timed_supervisor_spec(Id, Args) ->
  { Id, { timed_supervisor, start_link, [
    use_monitor,
    { shutdown, 600 * 1000 }
  | Args ] }, transient, 600 * 1000, worker, [ ] }.

hashes_sync(Id, Args, Shedule) ->
  timed_supervisor_spec(Id, [ { biznip_hashes_sync, start_link, Args } , { shedule, Shedule } ]).
