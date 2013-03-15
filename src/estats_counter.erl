%% Copyright
-module(estats_counter).

-type(tid() :: integer()).

%% API
-export([inc/2, inc/3, get_value/2]).

-spec inc(Table :: tid(), Key :: tuple()) -> ok.
inc(Table, Key) -> inc(Table, Key, 1).

-spec inc(Table :: tid(), Key :: tuple(), Step :: term() ) -> ok | { error, Reason :: term() }.
inc(Table, Key, Step) when is_integer(Step) ->
  try dets:update_counter(Table, Key, Step) of
    _ -> ok
  catch
    error:badarg -> dets:insert(Table, { Key, Step }),
        ok
  end,
  ok;

inc(_Table, _Key, {_Pos, 0 } ) -> ok;

inc(Table, Key, {Pos, Step} ) ->
  try dets:update_counter(Table, Key, {Pos + 1, Step}) of
    _ -> ok
  catch
    error:badarg ->
      Values = case dets:member(Table, Key) of
        false ->
          [ Key ] ++  [ 0 || _ <- lists:seq(2,Pos) ] ++ [ Step ];
        true ->
          [ Val ] = dets:lookup(Table, Key),
          List = tuple_to_list(Val),
          [ Key ] ++ List ++ [ 0 || _ <- lists:seq(2,max(1,Pos-size(Val)-1)) ] ++ [ Step ]
      end,
      dets:insert(Table, list_to_tuple(Values))
  end;


inc(Table, Key, Steps) when is_list(Steps) ->
  { List, _ } = lists:mapfoldl(fun(Step, Index) ->
    { { Index, Step } , Index + 1}
  end, 1, Steps),
  Pairs = lists:reverse(List),
  lists:map(fun(Pair) ->
    ok = inc(Table, Key, Pair)
  end, Pairs),
  ok.


get_value(Table, Key) ->
  case dets:lookup(Table, Key) of
    [ ] -> 0;
    [ { Key, Value } ] -> Value;
    [ Tuple ] when is_tuple(Tuple) ->
      [ _ | Values ] = tuple_to_list(Tuple),
      Values
  end.

