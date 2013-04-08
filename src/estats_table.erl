-module(estats_table).

%% API
-export([lookup/3]).

lookup(_, Table, _ ) when Table == undefined -> [ ];
lookup(ets, Table, Key) ->
  ets:lookup(Table, Key);
lookup(dets, Table, Key) ->
  dets:lookup(Table, Key).
