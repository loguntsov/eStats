%% Copyright
-module(date).
-author("begemot").

-include("include/types.hrl").

%% API
-export([year/1, month/1, day/1, is_valid_date/1, gregorian_days/1, period_to_list/1, date_next_days/2]).

-spec year(Date :: date()) -> integer().
year({Year, _ , _ }) -> Year.

-spec month(Date :: date()) -> integer().
month({_, Month , _ }) -> Month.

-spec day(Date :: date()) -> integer().
day({_, _ , Day }) -> Day.

-spec is_valid_date(Date :: date()) -> boolean().
is_valid_date(Date) ->
  calendar:valid_date(Date).

-spec gregorian_days(Dates :: [ date() ]) -> [ integer() ].
gregorian_days(Dates) ->
  [ calendar: date_to_gregorian_days(Date) || Date <- Dates ].

-spec is_less(Date1 :: date(), Date2 :: date()) -> boolean().
is_less(Date1, Date2) ->
  (calendar:date_to_gregorian_days(Date1) - calendar:date_to_gregorian_days(Date2)) < 0.

-spec date_next_days(Date :: date(), Days :: integer() ) -> date().
date_next_days(Date, 0) -> Date;
date_next_days(Date, Days) ->
  calendar:gregorian_days_to_date(calendar:date_to_gregorian_days(Date) + Days).

-spec period_to_list(Period :: date_period() ) -> {ok, [ date() ]} | { error, Reason :: any()}.
period_to_list({From, To} = _Period) ->
  Rule1 = is_valid_date(From),
  Rule2 = is_valid_date(To),
  Rule3 = is_less(From,To),
  if
    not Rule1  -> {error, datefrom_not_valid };
    not Rule2 -> { error, dateto_not_valid };
    Rule3 -> period_to_list({To, From});
    true -> {ok, period_to_list(calendar:date_to_gregorian_days(From),calendar:date_to_gregorian_days(To), [])}
  end.

period_to_list(Greg_From, Greg_To, Acc ) when Greg_From =:= Greg_To -> [ calendar:gregorian_days_to_date(Greg_From) | Acc];

period_to_list(Greg_From, Greg_To, Acc ) ->
  period_to_list(Greg_From + 1, Greg_To, [ calendar:gregorian_days_to_date(Greg_From) | Acc]).

