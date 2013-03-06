-module(estats_app).

-behaviour(application).

%% application callbacks
-export([start/0, start/2, stop/1]).

-behaviour(supervisor).

-export([init/1]).

start() ->
	ok = application:start(gproc),
  ok = application:start(crypto),
  ok = application:start(ranch),
	ok = application:start(cowboy),
	ok = application:start(estats),
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
  {http_port, HttpPort} = proplists:lookup( http_port, Options),

  Dispatch = cowboy_router:compile([
    {'_', [
      {"/", estats_cowboy_http_handler, []}
    ]}
  ]),
  {ok, _} = cowboy:start_http(http, 10, [{port, HttpPort}], [
    {env, [{dispatch, Dispatch}]}
  ]),

	{ ok, {{one_for_one, 1, 1000}, [
    { estats_storage, { estats_storage, start_link, [ ] }, permanent, 5000 , worker, [] },
    { estats_sub_server, { estats_sub_server, start_link, [{redis, Redis}, Path] }, permanent, 5000, worker, [] }
	]}}.


