
-ifndef(TYPES_HRL).
-define(TYPES_HRL, 1).

-type( tid() :: integer() | atom() ).
-type( date() :: {year(), month(), day()}).
-type( year() :: integer()).
-type( month() :: 1..12).
-type( day() :: 1..31).
-type( hour() :: 0..23).
-type( date_period() :: { From :: date(), To :: date() }).
-type( date_list() :: [ date() ]).

-endif. % TYPES_HRL

