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
	{ok, Pid}.

%%----------------------------------------------------------------------
%% Func: stop/1
%% Returns: any
%%----------------------------------------------------------------------
stop(_State) -> ok.

init(Options) ->
  Redis = proplists:get_value( redis, Options ),
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

  Childs = lists:merge([ [
    { estats_storage, { estats_storage, start_link, [ ] }, permanent, 5000 , worker, [] },
    { estats_offer_server, { estats_offer_server, start_link, [ Path ] }, permanent, 5000 , worker, [] } ],
    case Redis of
      undefined -> [];
      _ ->
        Connections = lists:duplicate(proplists:get_value(connections, Redis, 1), Redis),
        [{ estats_redis_sup, { estats_redis_sup, start_link, [ Connections ] }, permanent, 5000, supervisor, []}]
    end
  ]),

  { ok, {{one_for_one, 1, 1000}, Childs }}.


