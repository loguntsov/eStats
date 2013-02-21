-module(estats_app).

-behaviour(application).

%% application callbacks
-export([start/0, start/2, stop/1]).

-behaviour(supervisor).

-export([init/1]).

start() ->
	%application:start(gproc),
	%application:start(cowboy),
	application:start(estats),
	ok.

start(_Type, _StartArgs) ->
%	{ok, [Options]} = file:consult("erlycounter.conf"),
%	{ok, Pid} = supervisor:start_link(?MODULE, Options),
	io:format("eStats started"),
  Pid = self(),
	{ok, Pid}.

%%----------------------------------------------------------------------
%% Func: stop/1
%% Returns: any
%%----------------------------------------------------------------------
stop(_State) -> ok.

init(_Options) ->

%	{ ok, {{one_for_one, 5, 1000}, [
%		{ecsaver_pool, { ecsaver, pool_start, [ Save_process ] }, permanent, 5000 , supervisor, [] },
%		{ectable_sup, { ectable_sup, start_link, [ server_id, Tables, Save_time*1000, Command ] }, permanent, 5000 , supervisor, [] },
%		{ecserver_sup, { ecserver_sup, start_link, [ server_id, Servers, Tables ] }, permanent, 5000 , supervisor, dynamic },
%		{udp_server_sup, { udp_server_sup, start_link, [ server_id, Servers, element(1,Port_range), element(2, Port_range)] }, permanent, 5000 , supervisor, dynamic }
%	]}}.
ok.

