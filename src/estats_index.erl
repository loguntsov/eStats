%% Copyright
-module(estats_index).

-type(tid() :: integer()).

-export([search/2, is_exists/3, put/3]).

-spec put(Table :: tid(), Key :: any(), Value :: any()) -> ok.
put(Table, Key, Value) ->
  dets:insert(Table, {Key, Value}),
  ok.

-spec search(Table :: tid(), Key :: term()) -> [ any() ].
search(Table, Key) ->
  [ element(2, X) || X <- ets:lookup(Table, Key)].

-spec is_exists(Table :: tid(), Key :: term(), Value :: term()) -> boolean().
is_exists(Table, Key, Value) ->
  length(ets:match(Table, { Key, Value })) > 0.

% -spec timestamp() -> integer().
% timestamp() ->
%  { MegaSec, Sec, _ } = os:timestamp(),
%  MegaSec * 1000000 + Sec.
