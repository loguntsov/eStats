%% Copyright
-module(estats_counter).

-type(tid() :: integer()).

%% API
-export([inc/2, inc/3, get_value/2]).

-spec inc(Table :: tid(), Key :: tuple()) -> ok.
inc(Table, Key) -> inc(Table, Key, 1).

-spec inc(Table :: tid(), Key :: tuple(), Value :: integer()) -> ok.
inc(Table, Key, Step) ->
  try dets:update_counter(Table, Key, Step) of
    _ -> ok
  catch
    error:badarg -> dets:insert(Table, { Key, Step }),
        ok
  end,
  ok.

get_value(Table, Key) ->
  case dets:lookup(Table, Key) of
    [ ] -> 0;
    [ { Key, Value }] -> Value
  end.

