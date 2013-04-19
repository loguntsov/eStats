%% Copyright
-module(estats_report).

-include("include/report_info.hrl").

-include_lib("kernel/include/file.hrl"). % read_file_info

%% API
-export([
  map_save/4, map_load/3, map_load/2, map_load_list/2, map_hash/1, map_hash_list/1, index_add/3, index_add_limit/4, subkey/2, index_get/2,
  counter_inc/3, counter_inc/4, counter_get/2, counters_list_get/2, subkey_list/2, subkey_list/1,
  index_get_all/3, index_get_all/2, index_lookup/2, group/2, subkey_swap/2, open/3, close/1, files_size/1, info/1, create_ets/1,
  key_transform/2, format_key/2, format_tuple/2,
  sort_by_value/2, value_sum/1, is_ets_empty/1
]).

-spec open(module(), Path :: string(), Options :: proplist()) -> {ok, report_info}.
open(Module, Path, Options) ->
  Id = make_ref(),
  Mode = case proplists:lookup(mode, Options) of
    { mode, readonly } -> readonly;
    _ -> write_mode
  end,
%%   SaveInterval = case proplists:lookup( save_interval, Options ) of
%%     { save_interval, Interval } -> Interval * 1000;
%%     _ -> 60000
%%   end,
  SaveInterval = infinity,
  {ok, Counter_dets } = dets:open_file({counter, Id} , [
    { file, table_name(Path, counter) },
    { type, set },
    { access, case Mode of readonly -> read; _ -> read_write end },
    { min_no_slots, 100000 },
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
    { min_no_slots, 100000 },
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
    { min_no_slots, 100000 },
    { max_no_slots, default },
    { auto_save, SaveInterval }, % 10 минут интервал
    { ram_file, Mode =/= readonly },
    { keypos , 1 },
    { repair, true }
  ]),
  {ok, #report_info {
        module = Module,
        counters = undefined,
        map = undefined,
        index = undefined,
        path = Path,
        counters_data = Counter_dets,
        map_data = Map_dets,
        index_data = Index_dets
  }}.

close(Report) ->
  dets:close(Report#report_info.counters_data),
  dets:close(Report#report_info.index_data),
  dets:close(Report#report_info.map_data).


-spec is_ets_empty(Report :: report_info) -> boolean() | undefined.
is_ets_empty(Report) when
  Report#report_info.counters =:= undefined;
  Report#report_info.map =:= undefined; Report#report_info.index =:= undefined -> undefined;

is_ets_empty(Report) -> not(
  ets:info(Report#report_info.counters, size) > 0 andalso
  ets:info(Report#report_info.map, size) > 0 andalso
  ets:info(Report#report_info.index, size) > 0).

create_ets(Report) -> Report,
  Report#report_info{
    counters = ets:new(none, [ set ]),
    map = ets:new(none, [ set ]),
    index = ets:new(none, [ bag ])
  }.

-spec table_name(Path :: string(), Suffix::atom()) -> string().
table_name(Path, Suffix) ->
  lists:concat([Path, "/", Suffix]).

-spec info(Report :: report_info ) -> [ { Table_type:: atom(), [ { Prop_name :: atom(), Value :: term() } ] } ].
info(Report) ->
  [ { counter, dets:info(Report#report_info.counters_data) } ,
    { map, dets:info(Report#report_info.map_data) },
    { index, dets:info(Report#report_info.index_data) }
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


-spec counter_inc(report_info, counter_subkey(), counter_subkey()) -> ok.
counter_inc(Report, Key, Subkey) ->
  counter_inc(Report, Key, Subkey, 1).

-spec counter_inc(report_info, counter_subkey(), counter_subkey(), counter_value()) -> ok.
counter_inc( _Report, _Key, [], _Step ) -> ok;

counter_inc(Report, Key, [ Subkey | Subkey_list ], Steps) ->
  index_add(Report, Key , Subkey),
  NewKey = subkey(Key, Subkey),
  estats_counter:inc(Report#report_info.counters, NewKey, Steps),
  counter_inc(Report, NewKey, Subkey_list, Steps).

-spec counter_get(report_info, counter_key())-> counter_value().
counter_get(Report, Key) ->
  case estats_counter:get_value(ets, Report#report_info.counters, Key) of
    0 -> estats_counter:get_value(dets, Report#report_info.counters_data, Key);
    Ets ->
      Dets = estats_counter:get_value(dets, Report#report_info.counters_data, Key),
      estats_counter:step_sum(Ets, Dets)
  end.

-spec counters_list_get(report_info, [ counter_key() ]) -> [ counter() ].
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
            Key0 = subkey(K, D),
            ets:member(R#report_info.map, Key0) orelse
              dets:member(R#report_info.map_data, Key0 )
          end };
        true -> infinity
      end,
  case index_add_limit(Report, L, Key, Hash) of
    limit -> limit;
    ok ->
      ets:insert(Report#report_info.map, { subkey(Key, Hash) , Data }),
      { ok, Hash };
    exists ->
      { exists, Hash }
  end.

map_load(Report, Key, Hash) ->
  map_load(Report, subkey(Key, Hash)).

map_load(Report, Key) ->
  case estats_table:lookup(ets, Report#report_info.map, Key ) of
    [ ] ->
        case estats_table:lookup(dets, Report#report_info.map_data, Key) of
          [ ] -> [ ];
          [ A ] -> A
        end;
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

index_add(Report, Key, Data) ->
  ets:insert(Report#report_info.index, { Key, Data } ),
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
      Count = case estats_table:lookup(ets, Report#report_info.map, CountKey) of
        [ ] -> case estats_table:lookup(dets, Report#report_info.map_data, CountKey) of
          [ ] -> 0;
          [ Term ] ->
            ets:insert(Report#report_info.map, Term),
            { _ , A } = Term,
            A
        end;
        [ {  _ , A } ] -> A
      end,

      if
        Count > Limit -> limit;
        true ->
          ok = index_add(Report, Key, Data),
          if
            Count =:= 0 ->
              true = ets:insert(Report#report_info.map, { CountKey, 1 });
            true ->
              ets:update_counter(Report#report_info.map, CountKey,  {2,1})
          end,
          ok
      end;
    true -> exists
  end.


index_lookup(Report, Key) ->
  lists:usort(estats_table:lookup(ets, Report#report_info.index, Key) ++ estats_table:lookup(dets, Report#report_info.index_data, Key)).

index_get(Report, Keys) when is_list(Keys)->
  [ subkey(Key0,Data0) || { Key0 , Data0 } <- lists:flatten([ index_lookup(Report, Key) || Key <- Keys ]) ].


index_get_all(Report, Key) ->
  index_get_all(Report, Key, []).

% deprecated
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

sort_by_value(Order_pos, List) when is_list(List) ->
  [ { Key, estats_counter:step_unpop(Order_pos, Value) } || { Key, Value } <-
      lists:keysort(2, [ {Key, estats_counter:step_pop(Order_pos, Value) } || { Key, Value } <- List ]) ].

% Произвести сложение значений с одинаковыми ключами.
-spec value_sum(List :: list()) -> list().
value_sum(List) ->
  Ets = ets:new(none, [ set ]),
  try
    lists:foldl(fun
      ({Key, Value}, _ ) ->
        case ets:lookup(Ets, Key) of
          [ ] ->
            ets:insert(Ets, { Key, estats_counter:step_expand(estats_click:step_max_length(),Value) });
          [ { Key, V } ] when is_list(V);is_integer(V) ->
            ets:insert(Ets, { Key, estats_counter:step_sum(estats_click:step_max_length(), Value, V) });
          [ { Key, V } ] ->
            ets:insert(Ets, { Key, V })
        end,
        none;
      ( _, _ ) -> none
    end, none, List),
    ets:foldl(fun(Term, Xs) ->
    [ Term | Xs ]
    end, [ ], Ets)
  after
    ets:delete(Ets),
    []
  end.
