%% Copyright
-module(estats_gen_report).

-include("include/report_info.hrl").
-include("include/click_info.hrl").

%% API
-export([new/0, map_save/3, map_load/3, map_list/2, index_add/3, subkey/2, index_get/2,
  counter_inc/3, counter_inc/4, counter_get/2, counters_list_get/2, subkey_list/2, subkey_list/1,
  index_get_all/3, index_lookup/2, group/2, subkey_swap/2
]).

-export([start_link/1, click/2, report/2]).

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3,terminate/2]).

-record(state,{
  tables :: report_info,
  report_module :: atom(),
  date_list :: date_list()
}).

-export([behaviour_info/1]).

behaviour_info(callbacks) ->
  [{handle_click,2},
    {handle_report, 2}
  ];

behaviour_info(_Other) ->
  undefined.



init({Module}) ->
  gproc:add_local_name(Module),
  {ok, #state{
    report_module = Module,
    tables = new()
  } };

init({Module, _Period}) ->
  {ok, #state {
    report_module = Module
%    tables = #report_info {
%      % Здесь надо загружать соответсвующие dets
%    }
} }.

-spec start_link(Module :: atom()) -> {ok, pid()}.
start_link(Module) ->
  gen_server:start_link(?MODULE, {Module}, []).

-spec click(Pid :: pid(), Click :: click_info) -> ok.
click(Pid, Click) ->
  gen_server:cast(Pid, {click, Click}).

-spec report(Pid :: pid(), Query :: any()) -> Reply :: any().
report(Pid, Query) ->
  gen_server:call(Pid, { report, Query}).

handle_cast({click, Click}, State) ->
  ets:insert(State#state.tables#report_info.index, { period , Click#click_info.date }),
  ok = (State#state.report_module):handle_click(Click, State#state.tables),
  {noreply, State}.

handle_call({report, Query}, _From, State) ->
  try
    {ok, Group, Reply} = (State#state.report_module):handle_report(Query, State#state.tables),
    {reply, {ok, group(Group,Reply) }, State}
  catch
    error:function_clause -> {reply, {error, report_unknown }, State};
    error:Reason -> { reply, {error, Reason}, State }
  end;

handle_call( state, _From, State) ->
  {reply, State#state.tables, State}.

handle_info(_, State) -> { noreply, State }.

code_change(_, State, _) -> { ok, State}.

terminate(_, _) -> ok.


-spec new() -> report_info.
new() -> #report_info{
  counters = estats_counter:new(),
  map = ets:new(undefined, [set , {read_concurrency, true }]),
  index = ets:new(undefined, [bag])
}.

counter_inc( Report, Key, [ Index_subkey | Index_key ] , Counter_key) ->
  index_add(Report, Key, Index_subkey),
  counter_inc(Report, subkey(Key, Index_subkey), Index_key, Counter_key);

counter_inc( Report, Key, [], Counter_key) ->
  counter_inc(Report, Key, Counter_key).

counter_inc( _Report, _Key, []) -> ok;

counter_inc(Report, Key, [ Subkey | Subkey_list ]) ->
  index_add(Report, Key , Subkey),
  NewKey = subkey(Key, Subkey),
  estats_counter:inc(Report#report_info.counters, NewKey),
  counter_inc(Report, NewKey, Subkey_list).

counter_get(Report, Key) ->
  estats_counter:get_value(Report#report_info.counters, Key).

counters_list_get(Report, Keys) ->
  [ { Key , counter_get(Report, Key) } || Key <- Keys ].

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


index_lookup(Report, Key) ->
  ets:lookup(Report#report_info.index, Key).

index_get(Report, Keys) when is_list(Keys)->
  [ subkey(Key0,Data0) || { Key0 , Data0 } <- lists:flatten([ index_lookup(Report, Key) || Key <- Keys ]) ].


index_get_all(Report, Key, Subkey_list ) ->
  index_get(Report,subkey_list(Key, Subkey_list)).


group(0, Data) -> Data;
group(_Level, []) -> [];

group(Level, Data) ->

  NewLevel = Level - 1,

  [ {Key,
      [ case X of
        { [], Value } -> Value;
        { [Key0], Value} -> {Key0, Value};
        _ -> X
        end || X <- group(NewLevel, ValueList)] }
      || {Key, ValueList} <- group_pairs(lists:keysort(1,
          [ { Key, { Keys, Value } } || {[ Key | Keys ], Value} <- Data ]
      ))
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



-spec d_hash(Data :: any() ) -> integer().
d_hash(Data) ->
  erlang:phash(Data, 4294967295). % 2^32 - 1

