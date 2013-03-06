%% Copyright
-module(estats_cowboy_http_handler).

%% API

-export([init/3, handle/2, terminate/3]).

-behaviour(cowboy_http_handler).


init(_Transport, Req, []) ->
  {ok, Req, undefined}.

handle(Req, State) ->
  {Method, Req2} = cowboy_req:method(Req),
  HasBody = cowboy_req:has_body(Req2),
  {ok, Req3} = maybe_echo(Method, HasBody, Req2),
  {ok, Req3, State}.

maybe_echo(<<"POST">>, true, Req) ->
  {ok, PostVals, Req2} = cowboy_req:body_qs(Req),
  Data = proplists:get_value(<<"query">>, PostVals),
  echo(Data, Req2);

maybe_echo(_, _, Req) ->
  reply_html(Req).

echo(undefined, Req) ->
  reply_html(Req);

echo(<<"">>, Req) ->
  reply_html(Req);

echo(Json, Req) ->
  try
    Proplist = try
      jsx:decode(Json)
    catch
      error:badarg ->
        throw({error, json_bad})
    end,
    Module = erlang:binary_to_existing_atom(<<"estats_report_", (get_property(module, Proplist))/binary >>, utf8),
    case module_is_report(Module) of
      true -> ok;
      false -> throw({error, module_is_not_report})
    end,

    Type = try
      erlang:binary_to_existing_atom(get_property(type, Proplist), utf8)
    catch
      error:badarg -> throw({error, type_not_recognized})
    end,
    {Input, Output} = try
      {ok, In, Out} = Module:handle_info(Type),
      {In, Out}
    catch
      error:function_clause ->
        throw({error, type_not_recognized})
    end,

    Period = case get_property(period, Proplist) of
      [{<<"begin">>, DateBin_begin},{<<"end">>,DateBin_end}] when is_binary(DateBin_begin), is_binary(DateBin_end) ->
        DateBegin = date:from_sql_binary(DateBin_begin),
        DateEnd = date:from_sql_binary(DateBin_end),
        {ok, Date} = date:period_to_list({DateBegin, DateEnd}),
        Date;
      [{<<"begin">>, DateBin_begin},{<<"end">>,DateBin_end}] when is_list(DateBin_begin), is_list(DateBin_end)->
        DateBegin = date:from_proplists(DateBin_begin),
        DateEnd = date:from_proplists(DateBin_end),
        {ok, Date} = date:period_to_list({DateBegin, DateEnd}),
        Date;
      [ Element | _ ] = List when is_list(List), is_binary(Element) ->
        [ date:from_sql_binary(X) || X <- List ];
      [ Element | _ ] = List when is_list(List), is_list(Element) ->
        [ date:from_proplists(X) || X <- List ]
    end,

    Query = get_property('query', Proplist),

    QueryTuple = list_to_tuple(lists:reverse(lists:map(fun(Name) ->
      Param = erlang:atom_to_binary(Name, utf8),
      try
        { Param, Value } = proplists:lookup(Param, Query),
        Value
      catch
        error:{badmatch,none} -> throw({error, query_property_not_found, Param})
      end
    end, Input))),

    Data = [ {format_key(Key, Output), Value } || { Key, Value } <- lists:usort(
      case estats_offer_server:report(estats_offer_server:pid(), Module, Type, Period, QueryTuple ) of
        {error, Reason} -> throw({error, report_answer, Reason});
        {ok, D} -> D
      end
    )],


    BeforeJson = estats_report:group([ erlang:atom_to_binary(Item, utf8) || Item <- Output], Data),

    Answer = try
      jsx:encode(BeforeJson)
    catch
      error:badarg ->
        throw({error, bad_answer_for_json, BeforeJson})
    end,

    reply_html(Json, Answer, Req)
  catch
    throw:{error, json_bad} ->
      reply_html(Json, <<"Неверный формат JSON">>, Req);
    throw:{error, bad_answer_for_json, R} ->
      reply_html(Json, <<"Немогу преборазовать ответ в JSON: ", (list_to_binary(io_lib:format("~p",[R])))/binary >>, Req);
    throw:{error, property_not_exists, Name } ->
       reply_html(Json, <<"В запросе необходимо поле ", (erlang:atom_to_binary(Name))/binary >>, Req);
    throw:{error, module_is_not_report} ->
      reply_html(Json, <<"Указанный в запросе модуль не существует">>, Req);
    throw:{error, type_not_recognized} ->
      reply_html(Json, <<"Тип запроса к модулю отчета не распознан">>, Req);
    throw:{error, report_answer, R} ->
      reply_html(Json, <<"Отчет вернул следующую ошибку: ", (list_to_binary(lists:flatten(io_lib:format("~p",[R]))))/binary >>, Req);
    throw:{error, query_property_not_found, Prop} ->
      reply_html(Json, <<"Для данного отчета могу найти требуемое поле в запросе: ", Prop/binary >>, Req);
    error:R ->
      reply_html(Json, <<"В процессе обработки ошибки возникла следующая ошибка: ", (list_to_binary(lists:flatten(io_lib:format("~p",[R]))))/binary >>, Req)
  end.

terminate(_Reason, _Req, _State) ->
  ok.

reply_html(Query, Msg, Req) ->
  cowboy_req:reply(200,[{<<"content-type">>, <<"text/html;charset=utf-8">>}],<< <<"<html><body>">>/binary, Msg/binary, <<"<form method='post' action=''><textarea name='query' cols='80' rows='30'>">>/binary, Query/binary ,<<"</textarea><br/><button type='submit'>Отправить</button></form></body></html>">>/binary >>, Req).

reply_html(Req) ->
  Q = <<"{\"module\" : \"count\",
 \"type\" : \"affiliates_hour_count\",
 \"period\" : [\"2013-02-28\"],
 \"query\" : {\"offer_id\" : 1, \"affiliate_id\" : 1 }
}">>,

  reply_html(Q,<<"">>, Req).

%% reply_html(Query, Req) ->
%%   reply_html(Query, <<"">>, Req).



get_property(Name, Proplist) when is_atom(Name) ->
  try
    Bin = erlang:atom_to_binary(Name, utf8),
    { Bin, Result } = proplists:lookup(Bin, Proplist),
    Result
  catch
    error:{badmatch, none} -> throw({error, property_not_exists, Name})
  end.

module_is_report(Module) ->
  try
    true = is_atom(Module),
    {module, Module} = code:ensure_loaded(Module),
    {attributes, Attr } = proplists:lookup(attributes, Module:module_info()),
    {behaviour, Behav } = proplists:lookup(behaviour, Attr),
    true = lists:member(estats_gen_report, Behav),
    true
  catch
    _: _ -> false
  end.

format_key([], _) -> [];
format_key(_, []) -> [];
format_key([Key | Keys ], [ Label | Labels ] ) when Label =:= 'date' ->
  [ date:to_sql_binary(Key) | format_key(Keys, Labels) ];
format_key([Key | Keys ], [ _Label | Labels ] ) ->
  [ Key | format_key(Keys, Labels) ].
