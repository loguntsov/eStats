%% Copyright
-module(estats_cowboy_help_handler).

%% API
-export([init/3, handle/2, terminate/3]).

init(_Transport, Req, []) ->
  {ok, Req, undefined}.

terminate(_Reason, _Req, _State) ->
  ok.

handle(Req, State) ->
  {ok, Body} = file:read_file("help/reports.html"),
  {ok, Req1} = cowboy_req:reply(200, [{<<"content-type">>, <<"text/html;charset=utf-8">>}], Body, Req),
  {ok, Req1, State}.
