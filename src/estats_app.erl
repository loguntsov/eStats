-module(estats_app).

-behaviour(application).

%% application callbacks
-export([start/0, stop/0, restart/0, start/2, stop/1, prep_stop/1 ]).

-behaviour(supervisor).

-export([init/1]).

-record(state, { pid :: pid() }).

start() ->
	ok = application:start(gproc),
  ok = application:start(crypto),
  ok = application:start(ranch),
	ok = application:start(cowboy),
  ok = application:start(emysql),
	ok = application:start(estats),
	ok.

stop() ->
  application:stop(estats),
  application:stop(emysql),
  application:stop(cowboy),
  application:stop(ranch),
  application:stop(crypto),
  application:stop(gproc),
  ok.

start(_Type, _StartArgs) ->
	{ok, [Options]} = file:consult("estats.conf"),
	{ok, Pid} = supervisor:start_link(?MODULE, Options),
	io:format("eStats started~n"),
	{ok, Pid, #state{ pid = Pid } }.

%%----------------------------------------------------------------------
%% Func: stop/1
%% Returns: any
%%----------------------------------------------------------------------
prep_stop(State) ->
  supervisor:terminate_child(State#state.pid, estats_redis_sup),
  io:format("eStats wait 10 seconds ...~n"),
  timer:sleep(10000),
  io:format("eStats save cached info ...~n"),
  estats_subreport_sup:exit(),
  estats_subreport_sup:ensure_no_child(),
  State.

stop(_State) ->
  io:format("eStats stoped~n"),
  ok.

restart() ->
  lists:foldl(fun({App, _ , _}, _) ->
    case App of
      estats ->
        stop();
      _ -> ok
    end
  end,0,application:which_applications()),
  start().

init(Options) ->
  Redis = proplists:get_value( redis, Options ),
  Path = proplists:get_value( path, Options, "data/"),
  {http_port, HttpPort} = proplists:lookup( http_port, Options),
  {mysql, Db } = proplists:lookup(mysql, Options),

  Dispatch = cowboy_router:compile([
    {'_', [
      {"/", estats_cowboy_http_handler, []},
      {"/help", estats_cowboy_help_hangler, []}
    ]}
  ]),
  {ok, _} = cowboy:start_http(http, 10, [{port, HttpPort}], [
    {env, [{dispatch, Dispatch}]}
  ]),

  Childs = lists:merge([ [
    { mysql, { mysql, start_link, [Db] }, permanent, 600*1000, supervisor, []},
    { estats_storage, { estats_storage, start_link, [ ] }, permanent, 5000 , worker, [] },
    { estats_subreport_sup, { estats_subreport_sup, start_link, [ ] }, permanent, 600 * 1000, supervisor, [ ]},
    { estats_offer_server, { estats_offer_server, start_link, [ Path ] }, permanent, 5000 , worker, [] } ],
    case Redis of
      undefined -> [];
      _ ->
        Connections = lists:duplicate(proplists:get_value(connections, Redis, 1), Redis),
        [{ estats_redis_sup, { estats_redis_sup, start_link, [ Connections ] }, permanent, 5000, supervisor, []}]
    end
  ]),

  { ok, {{one_for_one, 1, 1000}, Childs }}.


