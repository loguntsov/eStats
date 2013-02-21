%% Copyright
-module(estats_report).

-include("include/report_info.hrl").

%% API
-export([new/0, map_save/3, map_load/3, map_list/2, index_add/3, subkey/2, index_get/2, counter_inc/3, counter_get/2, counters_list/2, subkey_list/2, index_get_all/3]).

-spec new() -> report_info.
new() -> #report_info{
  counters = estats_counter:new(),
  map = ets:new(undefined, [set , {read_concurrency, true }]),
  index = ets:new(undefined, [bag])
}.

counter_inc( _Report, _Key, []) -> ok;

counter_inc(Report, Key, [ Subkey | Subkey_list ]) ->
  index_add(Report, Key , Subkey),
  NewKey = subkey(Key, Subkey),
  estats_counter:inc(Report#report_info.counters, NewKey),
  counter_inc(Report, NewKey, Subkey_list).

counter_get(Report, Keys) when is_list(Keys) ->
  [ {Key, counter_get(Report, Key) } || Key <- Keys ];

counter_get(Report, Key) ->
  estats_counter:get_value(Report#report_info.counters, Key).

counters_list(Report, Key) ->
  [ { Subkey , counter_get(Report, subkey(Key, Subkey)) } || Subkey <- index_get(Report, Key) ].

% Построение ключа на основе ключа и подключа
subkey(Key, Subkey) when is_tuple(Key)->
  list_to_tuple(lists:reverse([Subkey|lists:reverse(tuple_to_list(Key))]));

subkey(Key, Subkey) -> { Key, Subkey }.

% Построение набора ключей путем перебора встречающихся [] в субключе

subkey_list(Key, []) -> Key;

subkey_list(Key, [Subkey | Subkey_list]) when is_list(Subkey), is_list(Key) ->
  subkey_list([ subkey(A,B) || A <- Key, B <- Subkey ], Subkey_list);

subkey_list(Key, [Subkey | Subkey_list]) when not(is_list(Subkey)), is_list(Key) ->
  subkey_list([ subkey(A,Subkey) || A <- Key ], Subkey_list);

subkey_list(Key, Subkey_list) -> subkey_list([Key],Subkey_list).




map_save(Report, Key, Data) ->
  Hash = d_hash(Data),
  ets:insert(Report#report_info.map, { { Key, Hash } , Data }),
  index_add(Report, Key, Hash),
  Hash.

map_load(Report, Key, Hash) ->
  case ets:lookup(Report#report_info.map, { Key, Hash}) of
    [ ] -> none;
    [ A ] -> A
  end.

map_list(Report, Key) when is_list(Key)->
  ets:lookup(Report#report_info.index, Key).

index_add(Report, Key, Data) ->
  ets:insert(Report#report_info.index, { Key, Data}),
  ok.


index_get(Report, Keys) when is_list(Keys)->
  [ subkey(Key,Data) || { Key , Data } <- lists:flatten([ ets:lookup(Report#report_info.index, Key) || Key <- Keys ]) ];

index_get(Report, Key) ->
  index_get(Report, [ Key ]).


index_get_all(Report, Key, []) ->
  error_logger:info_report("~p", Key),
  index_get(Report, Key);

index_get_all(Report, Key, [ Subkey | Subkey_list ] ) ->
  lists:flatten([ X || X <- index_get_all(Report, subkey_list(Key, [ Subkey ] ), Subkey_list) ]).

-spec d_hash(Data :: any() ) -> integer().
d_hash(Data) ->
  erlang:phash(Data, 4294967295). % 2^32 - 1