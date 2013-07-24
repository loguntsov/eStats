%% Copyright
-module(date).
-author("begemot").

-include("include/types.hrl").

%% API
-export([year/1, month/1, day/1, is_valid/1, is_less/2, gregorian_days/1, period_to_list/1, next_days/2, between/2, to_string/1, to_string/2,
  to_proplists/1, from_proplists/1, proplists_is_date/1, to_binary/1, to_binary/2,from_sql_binary/1, to_sql_binary/1,
  to_week_number/1, from_week_number/1, start_week/1,
  start_month/1, now/0, max/2
]).

-spec year(Date :: date()) -> integer().
year({Year, _ , _ }) -> Year.

-spec month(Date :: date()) -> integer().
month({_, Month , _ }) -> Month.

-spec day(Date :: date()) -> integer().
day({_, _ , Day }) -> Day.

-spec is_valid(Date :: date()) -> boolean().
is_valid(Date) ->
  calendar:valid_date(Date).

-spec gregorian_days(Dates :: [ date() ]) -> [ integer() ].
gregorian_days(Dates) ->
  [ calendar: date_to_gregorian_days(Date) || Date <- Dates ].

-spec next_days(Date :: date(), Days :: integer() ) -> date().
next_days(Date, 0) -> Date;
next_days(Date, Days) ->
  calendar:gregorian_days_to_date(calendar:date_to_gregorian_days(Date) + Days).

-spec period_to_list(Period :: date_period() ) -> {ok, [ date() ]} | { error, Reason :: any()}.
period_to_list({From, To}) ->
  Rule1 = is_valid(From),
  Rule2 = is_valid(To),
  Rule3 = is_less(From,To),
  if
    not Rule1  -> {error, datefrom_not_valid };
    not Rule2 -> { error, dateto_not_valid };
    not Rule3 -> period_to_list({To, From});
    true ->
      Date_from = calendar:date_to_gregorian_days(From),
      Date_to = calendar:date_to_gregorian_days(To),
      {ok, period_to_list(Date_from, Date_to - Date_from + 1)}
  end.

period_to_list(_ , Days ) when Days < 1 -> [ ];

period_to_list(Greg_From, Days ) ->
  [ calendar:gregorian_days_to_date(Greg_From) | period_to_list( Greg_From + 1, Days - 1 ) ].


between(Date, {Date_from, Date_to } = _Period) ->
  GDate = calendar:date_to_gregorian_days(Date),
  GDateFrom = calendar:date_to_gregorian_days(Date_from),
  GDateTo = calendar:date_to_gregorian_days(Date_to),
  (GDate >= GDateFrom) and (GDate =< GDateTo).

to_string({Year, Month, Day}, Delimiter) ->
  lists:concat([integer_to_list(Year), Delimiter, integer_to_list(Month), Delimiter, integer_to_list(Day)]).

to_string(Date) ->
  to_string(Date, "-").

from_proplists(Proplist) ->
  {year, Year } = proplists:lookup(year, Proplist),
  {month,Month } = proplists:lookup(month, Proplist),
  {day, Day } = proplist:lookup(day, Proplist),
  Date = { Year, Month, Day },
  case is_valid(Date) of
    true -> Date;
    false -> throw({error, is_not_date, Proplist})
  end.

to_proplists({Year, Month, Day}) ->
  [ { year, Year }, { month, Month }, { day, Day } ].

proplists_is_date(Proplists) ->
  try
    true = proplists:is_defined(year, Proplists),
    true = proplists:is_defined(year, Proplists),
    true = proplists:is_defined(year, Proplists),
    true
  catch
    error:badmatch -> false
  end.

to_binary({Year, Month, Day}, Delimiter) ->
  << (erlang:integer_to_binary(Year))/binary,
  Delimiter/binary,
  (erlang:integer_to_binary(Month))/binary,
  Delimiter/binary,
  (erlang:integer_to_binary(Day))/binary >>.

to_binary({Year, Month, Day}) ->
  to_binary({Year, Month, Day}, <<"-">>).

-spec from_sql_binary(Date :: binary()) -> date().
from_sql_binary(Binary) when is_binary(Binary) ->
    <<Year:4/binary-unit:8,
      _:1/binary-unit:8,
      Month:2/binary-unit:8,
      _:1/binary-unit:8,
      Day:2/binary-unit:8
      >> = Binary,
     { erlang:binary_to_integer(Year), erlang:binary_to_integer(Month), erlang:binary_to_integer(Day) }.

to_sql_binary({Year, Month, Day}) ->
  << (erlang:integer_to_binary(Year))/binary, <<"-">>/binary, (list_to_binary(io_lib:format("~2..0B-~2..0B", [Month, Day])))/binary >>.

to_week_number(Dates) when is_list(Dates) ->
  [ to_week_number(Date) || Date <- Dates ];

to_week_number(Date) ->
  (calendar:date_to_gregorian_days(Date) - 2) div 7.

from_week_number(Weeks) when is_list(Weeks) ->
  [from_week_number(Week) || Week <- Weeks];

from_week_number(Week) ->
  calendar:gregorian_days_to_date(Week * 7 + 2).

start_week(Dates) when is_list(Dates) ->
  [ start_week(Date) || Date <- Dates ];

start_week(Date) ->
  from_week_number(to_week_number(Date)).

start_month({Year, Month, _ }) ->
  {Year, Month, 1}.

-spec is_less(date()|undefined, date()|undefined) -> boolean().
is_less(undefined, _) -> false;
is_less(_, undefined) -> false;
is_less({Y0, _, _}, { Y1,_,_}) when Y0 < Y1 -> true;
is_less({Y0, M0, _}, { Y1,M1,_}) when Y0==Y1, M0 < M1 -> true;
is_less({Y0, M0, D0}, { Y1,M1,D1}) when Y0==Y1, M0==M1, D0 < D1 -> true;
is_less(_, _) -> false.


-spec now() ->
  { date(), { Hour :: integer(), Minute :: integer(), Second :: integer() } }.

now() ->
  TS = os:timestamp(),
  calendar:now_to_universal_time(TS).

max(Date1, Date2) ->
  case is_less(Date1, Date2) of
    true -> Date2;
    false -> Date1
  end.

