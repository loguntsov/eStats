%% Copyright
-module(estats_counter).

-type(tid() :: integer()).

%% API
-export([new/0, inc/2, inc/3, get_value/2]).

-spec new() -> tid().
new() ->
  ets:new(none, [ set , protected , compressed ]).

-spec inc(Table :: tid(), Key :: tuple()) -> ok.
inc(Table, Key) -> inc(Table, Key, 1).

-spec inc(Table :: tid(), Key :: tuple(), Value :: integer()) -> ok.
inc(Table, Key, Value) ->
  try ets:update_counter(Table, Key, [{ 2, Value }, { 3, 0, 0, timestamp() }]) of
    _ -> ok
  catch
    error:badarg -> ets:insert(Table, { Key, Value, timestamp() }),
        ok
  end,
  ok.

get_value(Table, Key) ->
  case ets:lookup(Table, Key) of
    [ ] -> 0;
    [ { Key, Value, _ }] -> Value
  end.

-spec timestamp() -> integer().
timestamp() ->
  { MegaSec, Sec, _MicroSec } = os:timestamp(),
  MegaSec * 1000000 + Sec.
