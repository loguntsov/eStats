%% Copyright
-module(estats_counter).

-type(tid() :: integer()).

%% API
-export([inc/2, inc/3, get_value/2, step_sum/2, step_sum/3]).

-spec inc(Table :: tid(), Key :: tuple()) -> ok.
inc(Table, Key) -> inc(Table, Key, 1).

-spec inc(Table :: tid(), Key :: tuple(), Step :: term() ) -> ok | { ok, Value :: integer() } | { error, Reason :: term() }.
inc(Table, Key, Step) when is_integer(Step) ->
  try dets:update_counter(Table, Key, Step) of
    Count -> {ok, Count}
  catch
    error:badarg -> dets:insert(Table, { Key, Step }),
      { ok, Step }
  end;

inc(_Table, _Key, {_Pos, 0 } ) -> ok;

inc(Table, Key, {Pos, Step} ) ->
  try dets:update_counter(Table, Key, {Pos + 1, Step}) of
    Count -> { ok, Count }
  catch
    error:badarg ->
      Values = case dets:member(Table, Key) of
        false ->
          [ Key ] ++  [ 0 || _ <- lists:seq(2,Pos) ] ++ [ Step ];
        true ->
          [ Val ] = dets:lookup(Table, Key),
          List = tuple_to_list(Val),
          List ++ lists:duplicate(max(0,Pos-(size(Val))),0) ++ [ Step ]
      end,
      ok = dets:insert(Table, list_to_tuple(Values)),
      { ok, Step }
  end;

inc(Table, Key, Steps) when is_list(Steps) ->
  { List, _ } = lists:mapfoldl(fun(Step, Index) ->
    { { Index, Step } , Index + 1}
  end, 1, Steps),
  Pairs = lists:reverse(List),
  lists:map(fun(Pair) ->
    inc(Table, Key, Pair)
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

step_sum(List1, List2, Number) ->
  step_sum(step_sum(List1, lists:duplicate(Number,0)),step_sum(List2, lists:duplicate(Number,0))).

step_sum([],[]) -> [];
step_sum([], [B | BList]) ->
  [ B | step_sum([], BList) ];
step_sum([A | AList],[]) ->
  [ A | step_sum(AList, []) ];
step_sum([A | AList ] , [ B | BList ]) ->
  [ A + B | step_sum(AList, BList) ];
step_sum(A, List) when not(is_list(A)), is_list(List) ->
  step_sum([A, 0], List);
step_sum(List, B) when not(is_list(B)), is_list(List) ->
  step_sum(List, [B,0]);
step_sum(A,B) ->
  [ A + B, 0 ].

