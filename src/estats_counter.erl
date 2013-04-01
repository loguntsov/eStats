%% Copyright
-module(estats_counter).

-type(tid() :: integer()).

%% API
-export([inc/2, inc/3, get_value/3, step_sum/2, step_sum/3, step_pop/2, step_unpop/2, step_expand/2, tuple_to_value_list/1]).

-spec inc(Table :: tid(), Key :: tuple()) -> ok.
inc(Table, Key) -> inc(Table, Key, 1).

-spec inc(Table :: tid(), Key :: tuple(), Step :: term() ) -> ok | { ok, Value :: integer() } | { error, Reason :: term() }.
inc(Table, Key, Step) when is_integer(Step) ->
  try ets:update_counter(Table, Key, Step) of
    Count -> {ok, Count}
  catch
    error:badarg -> ets:insert(Table, { Key, Step }),
      { ok, Step }
  end;

inc(_Table, _Key, {_Pos, 0 } ) -> ok;

inc(Table, Key, {Pos, Step} ) ->
  try ets:update_counter(Table, Key, {Pos + 1, Step}) of
    Count -> { ok, Count }
  catch
    error:badarg ->
      Values = case ets:member(Table, Key) of
        false ->
          [ Key ] ++  lists:duplicate(Pos-1, 0) ++ [ Step ];
        true ->
          [ Val ] = ets:lookup(Table, Key),
          List = tuple_to_list(Val),
          List ++ lists:duplicate(max(0,Pos-(size(Val))),0) ++ [ Step ]
      end,
      ets:insert(Table, list_to_tuple(Values)),
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

-spec get_value(Type :: ets | dets, Table :: term(), Key :: term()) -> integer() | list().
get_value(Type, Table, Key) ->
  case lookup(Type, Table, Key) of
    [ ] -> 0;
    [ { Key, Value } ] -> Value;
    [ Tuple ] when is_tuple(Tuple) ->
      tuple_to_value_list(Tuple)
  end.

lookup(ets, Table, Key) ->
  ets:lookup(Table, Key);
lookup(dets, Table, Key) ->
  dets:lookup(Table, Key).

tuple_to_value_list(Tuple) ->
  [ _ | Values ] = tuple_to_list(Tuple),
  Values.

step_sum(N, List1, List2) ->
  step_sum(step_expand(N, List1),step_expand(N, List2)).

step_expand(0, List) -> List;
step_expand(N, A) when not(is_list(A)) ->
  step_expand(N, [ A ]);
step_expand(N, List) when length(List) < N ->
  List ++ lists:duplicate(N - length(List), 0);
step_expand(_, List) -> List.

step_sum([],[]) -> [];
step_sum([], [B | BList]) when is_integer(B)->
  [ B | step_sum([], BList) ];
step_sum([A | AList],[]) when is_integer(A)->
  [ A | step_sum(AList, []) ];
step_sum([A | AList ] , [ B | BList ]) when is_integer(A), is_integer(B)->
  [ A + B | step_sum(AList, BList) ];
step_sum(A, List) when is_integer(A), is_list(List) ->
  step_sum([A, 0], List);
step_sum(List, B) when is_integer(B), is_list(List) ->
  step_sum(List, [B,0]);
step_sum(A,B) when is_integer(A), is_integer(B)->
  [ A + B, 0 ].

step_pop(0, List) -> List;
step_pop(1, List) -> List;
step_pop(N, List) ->
  L = step_expand(N, List),
  { Head , [ _ | Tail ] } = lists:split(N-1, L),
  [ lists:nth(N, L) | Head ++ Tail ].

step_unpop(0, List) -> List;
step_unpop(1, List) -> List;
step_unpop(N, [ Head | Tail ] = List) when N =< length(List) ->
  { List1, List2 } = lists:split(N-1, Tail),
  List1 ++ [Head] ++ List2.


