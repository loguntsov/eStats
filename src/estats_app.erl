-module(estats_app).

-behaviour(application).

%% application callbacks
-export([start/0, start/2, stop/1]).

-behaviour(supervisor).

-export([init/1]).

start() ->
	application:start(gproc),
	application:start(cowboy),
	application:start(estats),
	ok.

start(_Type, _StartArgs) ->
	{ok, [Options]} = file:consult("estats.conf"),
	{ok, Pid} = supervisor:start_link(?MODULE, Options),
	io:format("eStats started\n"),
  spawn_link(fun() -> t:t(), io:format("eStats inited\n") end),
	{ok, Pid}.

%%----------------------------------------------------------------------
%% Func: stop/1
%% Returns: any
%%----------------------------------------------------------------------
stop(_State) -> ok.

init(_Options) ->
	{ ok, {{one_for_one, 1, 1000}, [
    {estats_storage, { estats_storage, start_link, [ ] }, permanent, 5000 , worker, [] },
    {estats_offer_server, { estats_offer_server, start_link, [ "data" ] }, permanent, 5000 , worker, [] }
%		{ecsaver_pool, { ecsaver, pool_start, [ Save_process ] }, permanent, 5000 , supervisor, [] },
%		{ectable_sup, { ectable_sup, start_link, [ server_id, Tables, Save_time*1000, Command ] }, permanent, 5000 , supervisor, [] },
%		{ecserver_sup, { ecserver_sup, start_link, [ server_id, Servers, Tables ] }, permanent, 5000 , supervisor, dynamic },
%		{udp_server_sup, { udp_server_sup, start_link, [ server_id, Servers, element(1,Port_range), element(2, Port_range)] }, permanent, 5000 , supervisor, dynamic }
	]}}.


