-module(hash).

-export([encode/1]).

encode(List) when is_list(List) ->
  base64:encode(list_to_binary(lists:sublist(binary_to_list(crypto:md5(list_to_string(List))),15))).

list_to_string([]) -> [];
list_to_string([Item]) -> lists:flatten([to_binary(Item)]);
list_to_string([Item | List]) ->
  Binary = to_binary(Item),
  if
    is_list(Binary) ->
      Binary ++ [ <<":">> | list_to_string(List) ];
    true ->
      [ Binary, <<":">> | list_to_string(List) ]
  end.

to_binary(Value) when is_integer(Value) ->
  erlang:integer_to_binary(Value);

to_binary(Value) when is_list(Value) ->
  list_to_binary(Value);

to_binary(Value) when is_binary(Value) -> Value;

to_binary({Year, Month, Day}) when is_integer(Year), is_integer(Month), is_integer(Day) ->
  list_to_string([Year, Month, Day]).

