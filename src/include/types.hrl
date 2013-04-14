
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
-type( proplist() :: [ { Key :: term(), Value :: term() } ]).
-type( timestamp() :: {_MegaSecs :: integer(), _Secs :: integer(), _MicroSecs :: integer() } ).

-type(counter_value() :: integer() | [ integer() ]).
-type(counter_key() :: [ ]).
-type(counter() :: { counter_key(), counter_value() }).
-type(counter_subkey() :: counter_key()).
-endif. % TYPES_HRL

