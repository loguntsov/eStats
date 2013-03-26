%% Copyright
-module(estats_report).

-include("include/report_info.hrl").

-include_lib("kernel/include/file.hrl"). % read_file_info

%% API
-export([
  map_save/4, map_load/3, map_load/2, map_load_list/2, map_list/2, map_hash/1, map_hash_list/1, index_add/3, index_add_limit/4, index_size/2, subkey/2, index_get/2,
  counter_inc/3, counter_inc/4, counter_get/2, counters_list_get/2, subkey_list/2, subkey_list/1,
  index_get_all/3, index_lookup/2, group/2, subkey_swap/2, open/2, close/1, files_size/1, info/1, sync/1,
  key_transform/2, format_key/2, format_tuple/2
]).

-spec open(Path :: string(), Options :: proplist()) -> {ok, report_info}.
open(Path, Options) ->
  Id = make_ref(),
  Mode = case proplists:lookup(mode, Options) of
    { mode, readonly } -> readonly;
    _ -> write_mode
  end,
  SaveInterval = case proplists:lookup( save_interval, Options ) of
    { save_interval, Interval } -> Interval * 1000;
    _ -> 60000
  end,
  {ok, Counter_dets } = dets:open_file({counter, Id} , [
    { file, table_name(Path, counter) },
    { type, set },
    { access, case Mode of readonly -> read; _ -> read_write end },
    { min_no_slots, 2048 },
    { max_no_slots, default },
    { auto_save, SaveInterval }, % 10 минут интервал
    { ram_file, Mode =/= readonly },
    { keypos , 1 },
    { repair, true }
  ]),
  {ok, Map_dets } = dets:open_file({map, Id}, [
    { file, table_name(Path, map) },
    { type, set },
    { access, case Mode of readonly -> read; _ -> read_write end },
    { min_no_slots, 2048 },
    { max_no_slots, default },
    { auto_save, SaveInterval }, % 10 минут интервал
    { ram_file, Mode =/= readonly },
    { keypos , 1 },
    { repair, true }
  ]),
  {ok, Index_dets } = dets:open_file({index, Id}, [
    { file, table_name(Path, index) },
    { type, bag },
    { access, case Mode of readonly -> read; _ -> read_write end },
    { min_no_slots, 2048 },
    { max_no_slots, default },
    { auto_save, SaveInterval }, % 10 минут интервал
    { ram_file, Mode =/= readonly },
    { keypos , 1 },
    { repair, true }
  ]),
  {ok, #report_info {
        path = Path,
        counters = Counter_dets,
        map = Map_dets,
        index = Index_dets
  }}.

-spec close(Report :: report_info) -> ok.
close(Report) ->
  ok = dets:close(Report#report_info.counters),
  ok = dets:close(Report#report_info.map),
  ok = dets:close(Report#report_info.index),
  ok.

sync(Report) ->
  ok = dets:sync(Report#report_info.counters),
  ok = dets:sync(Report#report_info.map),
  ok = dets:sync(Report#report_info.index),
  ok.

-spec table_name(Path :: string(), Suffix::atom()) -> string().
table_name(Path, Suffix) ->
  lists:concat([Path, "/", Suffix]).

-spec info(Report :: report_info ) -> [ { Table_type:: atom(), [ { Prop_name :: atom(), Value :: term() } ] } ].
info(Report) ->
  [ { counter, dets:info(Report#report_info.counters) } ,
    { map, dets:info(Report#report_info.map) },
    { index, dets:info(Report#report_info.index) }
  ].

-spec files_size(Report :: report_info | string() )  -> { ok, non_neg_integer() } | { error, undefined }.
files_size(Report) when is_record(Report, report_info) ->
  {ok, lists:sum(lists:flatten([ [  Size || { file_size, Size } <- Info, is_integer(Size) ] || { _Type_table, Info } <- info(Report) ] ))} ;

files_size(Path) when is_list(Path) ->
  case file:list_dir(Path) of
    {ok , Filenames } ->
      FilesSize = lists:sum(lists:map(fun(Filename) ->
        { ok, Info } = file:read_file_info(lists:concat([Path, "/", Filename])),
        Info#file_info.size
      end, Filenames)),
      {ok, FilesSize};
    { error, _Reason } -> { error, undefined }
  end.



counter_inc(Report, Key, Subkey) ->
  counter_inc(Report, Key, Subkey, 1).

counter_inc( _Report, _Key, [], _Step ) -> ok;

counter_inc(Report, Key, [ Subkey | Subkey_list ], Steps) ->
  index_add(Report, Key , Subkey),
  NewKey = subkey(Key, Subkey),
  estats_counter:inc(Report#report_info.counters, NewKey, Steps),
  counter_inc(Report, NewKey, Subkey_list, Steps).

counter_get(Report, Key) ->
  estats_counter:get_value(Report#report_info.counters, Key).

counters_list_get(Report, Keys) ->
  [ { Key, counter_get(Report, Key) } || Key <- Keys ].

% Построение ключа на основе ключа и подключа
%subkey(Key, Subkey) when is_tuple(Key) ->
%  list_to_tuple(lists:reverse([Subkey|lists:reverse(tuple_to_list(Key))]));

subkey(Key, SubkeyList) when is_list(Key), is_list(SubkeyList) ->
  Key ++ SubkeyList;

subkey(Key, Subkey) when is_list(Key) ->
  Key ++ [Subkey];

subkey(Key, Subkey) when not(is_list(Key)), is_list(Subkey) ->
  [Key] ++ Subkey;

subkey(Key, Subkey) -> [ Key, Subkey ].


% Построение набора ключей путем перебора встречающихся [] в субключе

subkey_list(Key, Subkey) ->
  FullKey = subkey(Key, Subkey),
  subkey_list(FullKey).

subkey_list(Keys) ->
  subkey_list_acc(Keys,[]).

subkey_list_acc([], Acc) -> Acc;

subkey_list_acc(Key, []) when not(is_list(Key)) -> [ [ Key ] ];

subkey_list_acc([ Key | KeysList], []) when is_list(Key) ->
  subkey_list_acc(KeysList, Key);

subkey_list_acc([ Key | KeysList] , Acc) when is_list(Key) ->
  subkey_list_acc(KeysList,[ subkey(X,Y) || X <- Acc, Y <- Key ]);

subkey_list_acc([ Key | KeysList], []) when not(is_list(Key)) ->
  subkey_list_acc(KeysList, [[Key]]);

subkey_list_acc([ Key | KeysList] , Acc) ->
  subkey_list_acc(KeysList,[ subkey(X, Key) || X <- Acc ]).



% Поменять элменты ключа местами
subkey_swap(Key, []) -> Key;

subkey_swap(Key, Changes) when is_list(Key),is_list(Changes) ->
  [ lists:nth(X, Key) || X <- Changes ].

% Трансформировать ключи с помощью функции
-spec key_transform(Func :: function(), List :: list()) -> list().
key_transform(Func, List) when is_list(List), is_function(Func) ->
  lists:map(fun({Key, Value}) ->
    { Func(Key), Value }
  end, List).

%% Сохранить значение Data в словаре и получить его хеш. Limit - указывает как много можно сохранить в массиве значений по ключу Key
-spec map_save(Report :: report_info, Limit :: integer() | infinity, Key :: term(), Data :: term()) -> limit | { ok | exists, Hash :: integer() }.
map_save(Report, Limit, Key, Data) ->
  Hash = map_hash(Data),
  L = if
        is_integer(Limit) ->
          { Limit, fun(R, K, D) ->
            dets:member(R#report_info.map, subkey(K, D))
          end };
        true -> infinity
      end,
  case index_add_limit(Report, L, Key, Hash) of
    limit -> limit;
    ok ->
      ok = dets:insert(Report#report_info.map, { subkey(Key, Hash) , Data }),
      { ok, Hash };
    exists ->
      { exists, Hash }
  end.


map_load(Report, Key, Hash) ->
  map_load(Report, subkey(Key, Hash)).

map_load(Report, Key) ->
  case dets:lookup(Report#report_info.map, Key ) of
    [ ] -> none;
    [ A ] -> A
  end.

map_load_list(Report, Keys) when is_list(Keys) ->
  [ map_load(Report, Key) || Key <- Keys ].

map_hash(Term) ->
  erlang:phash2(Term, 4294967296). % 2^32

-spec map_hash_list(List :: list()) -> list().
map_hash_list(List) when is_list(List) ->
  [ map_hash(X) || X <- List ];

map_hash_list(Term) ->
  map_hash_list([Term]).

map_list(Report, Key) when is_list(Key)->
  dets:lookup(Report#report_info.index, Key).

index_add(Report, Key, Data) ->
  ok = dets:insert(Report#report_info.index, { Key, Data } ),
  ok.

%% Добавить Data в индекс Key с учетом лимита Limit
-spec index_add_limit(Report :: report_info, Limit :: integer() | infinity, Key :: term(), Data :: term()) ->
  exists | %% Значение Data в индексе Key уже есть
  limit | %% Значение в индекс не добавлено, т.к. превышен лимит для данного ключа
  ok.   %% В индекс добавлено новое значение
index_add_limit(Report, infinity, Key, Data) ->
  index_add(Report, Key, Data);

index_add_limit(Report, { Limit, ExistsFun }, Key, Data) when is_integer(Limit), is_function(ExistsFun,3) ->
  case ExistsFun(Report, Key, Data) of
    false ->
      CountKey = { index_count, Key },
      Count = case dets:lookup(Report#report_info.map, CountKey) of
        [ ] -> 0;
        [ {  _ , A } ] -> A
      end,

      if
        Count > Limit -> limit;
        true ->
          ok = index_add(Report, Key, Data),
          if
            Count =:= 0 ->
              ok = dets:insert(Report#report_info.map, { CountKey, 1 });
            true ->
              dets:update_counter(Report#report_info.map, CountKey, 1)
          end,
          ok
      end;
    true -> exists
  end.

-spec index_size(Report :: report_info, Key :: term()) -> integer().
index_size(Report, Key) ->
  case dets:lookup(Report#report_info.map, { index_count, Key }) of
    [ ] ->
      length(index_get(Report, Key ));
    [ { _, Count } ] when is_integer(Count) -> Count
  end.


index_lookup(Report, Key) ->
  dets:lookup(Report#report_info.index, Key).

index_get(Report, Keys) when is_list(Keys)->
  [ subkey(Key0,Data0) || { Key0 , Data0 } <- lists:flatten([ index_lookup(Report, Key) || Key <- Keys ]) ].


index_get_all(Report, Key, Subkey_list ) ->
  index_get(Report,subkey_list(Key, Subkey_list)).


group([], Data) -> Data;
group([ Label ], Data ) when is_list(Data) ->
  [ case Item of
      { [ A ], B } -> [{ Label, A }, { <<"value">>, B }];
      { A, B } -> [{ Label, A }, { <<"value">>, B }];
      A when is_integer(A) -> A;
      B when is_binary(B) -> B
    end || Item <- Data];
group([Label | Labels], Data ) ->

  [ [ { Label, Key}, { <<"value">>,
    [ case X of
      { [], Value } -> Value;
      { [Key0], Value} -> group(Labels, [{Key0,Value}]);
      _ -> X
    end || X <- group(Labels, ValueList)
    ] } ] || {Key, ValueList} <- group_pairs(
    [ { Key, { Keys, Value } } || {[ Key | Keys ], Value} <- Data ]
  )
  ].

%% Взято отсюда: https://github.com/arcusfelis/lists2/blob/master/src/lists2.erl#L286
%% @doc Group pair values by key.
%% It is `map_group_with(fun(X) -> X end, List)'.
group_pairs([]) ->
  [];

group_pairs(List) ->
  [{SortedHKey, SortedHValue}|SortedT] = lists:keysort(1, List),
  group_reduce(SortedT, SortedHKey, [SortedHValue]).


%% @doc Return `[{Key, [Value1, Value2, ...]}]'.
%% @end
%%
%% Still the same group:
%% group_reduce([{<<"user">>,{x_prefix_name,user,65,true,true,true}}],
%%                <<"author">>,[{x_prefix_name,author,65,true,false,true}])
%%
%% Version for ppairs.
group_reduce([{Key, Val}|T], Key, Vals) ->
  group_reduce(T, Key, [Val|Vals]);

%% Add a new group:
group_reduce([{NewKey, Val}|T], OldKey, Vals) ->
  [{OldKey, lists:reverse(Vals)} | group_reduce(T, NewKey, [Val])];

group_reduce([], Key, Vals) ->
  [{Key, lists:reverse(Vals)}].

% Форматировать элемент ключа в соотвествии с его типом
-spec format_key( Key :: list(), Labels :: list()) -> list().
format_key([], _) -> [];
format_key(Key, []) -> Key;
format_key([Key | Keys ], [ Label | Labels ] ) when Label =:= 'date' ->
  [ date:to_sql_binary(Key) | format_key(Keys, Labels) ];
format_key([Key | Keys ], [ _Label | Labels ] ) ->
  [ Key | format_key(Keys, Labels) ];
format_key(Key, [Label]) ->
  format_key([Key],[Label]).

format_tuple(TupleList, Labels) ->
  lists:map(fun({Key, Value}) ->
    { format_key(Key, Labels) , Value }
  end, TupleList).
