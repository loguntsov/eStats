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
  %spawn_link(fun() -> t:t(), io:format("eStats inited\n") end),
	{ok, Pid}.

%%----------------------------------------------------------------------
%% Func: stop/1
%% Returns: any
%%----------------------------------------------------------------------
stop(_State) -> ok.

init(Options) ->
  {redis, Redis } = proplists:lookup( redis, Options ),
  {path, Path} = proplists:lookup( path, Options),
	{ ok, {{one_for_one, 1, 1000}, [
    { estats_storage, { estats_storage, start_link, [ ] }, permanent, 5000 , worker, [] },
    { estats_sub_server, { estats_sub_server, start_link, [{redis, Redis}, Path] }, permanent, 5000, worker, [] }
	]}}.


