%% Copyright
-module(estats_index).

-type(tid() :: integer()).

-export([new/0, search/2, is_exists/3, put/3]).

-spec new() -> tid().
new() ->
  ets:new(undefined, [ bag , protected ]).

-spec put(Table :: tid(), Key :: any(), Value :: any()) -> ok.
put(Table, Key, Value) ->
  ets:insert(Table, {Key, Value}),
  ok.

-spec search(Table :: tid(), Key :: term()) -> [ any() ].
search(Table, Key) ->
  lists:map(fun(Item) -> element(2, Item) end, ets:lookup(Table, Key)).

-spec is_exists(Table :: tid(), Key :: term(), Value :: term()) -> boolean().
is_exists(Table, Key, Value) ->
  length(ets:match(Table, { Key, Value })) > 0.

% -spec timestamp() -> integer().
% timestamp() ->
%  { MegaSec, Sec, _ } = os:timestamp(),
%  MegaSec * 1000000 + Sec.
