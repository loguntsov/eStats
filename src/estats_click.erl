%% Copyright
-module(estats_click).

-include("include/click_info.hrl").

%% API
-export([from_json/1, uniq_step/1]).

-spec from_json(Json :: binary()) -> {ok, click_info} | { error, Reason :: term() }.
from_json(Json) ->
  try
    Proplist = jsx:decode(Json),

    Year = from_binary(proplists:get_value(<<"date_year">>, Proplist, undefined)),
    Month = from_binary(proplists:get_value(<<"date_month">>, Proplist, undefined)),
    Day = from_binary(proplists:get_value(<<"date_day">>, Proplist, undefined)),

    Date = {Year, Month, Day},

    Subid = lists:map(fun(Number) ->
      { Number, from_binary(proplists:get_value(<< <<"aff_subid">>/binary, (integer_to_binary(Number))/binary >>, Proplist, <<"">>)) }
    end, lists:seq(1,5)),
    case date:is_valid(Date) of
      false -> throw({error, date_not_valid});
      true ->
        Click = #click_info{
            id = from_binary(proplists:get_value(<<"id">>, Proplist, undefined)),
            offer_id = from_binary(proplists:get_value(<<"offer_id">>, Proplist, undefined)),
            offer_url_id = from_binary(proplists:get_value(<<"offer_id">>, Proplist, undefined)),
            advertiser_id = from_binary(proplists:get_value(<<"advertiser_id">>, Proplist, undefined)),
            affiliate_id = from_binary(proplists:get_value(<<"affiliate_id">>, Proplist, undefined)),
            hour = from_binary(proplists:get_value(<<"date_hour">>, Proplist, undefined)),
            year = Year,
            month = Month,
            day = Day,
            date = Date,
            day_number = calendar:date_to_gregorian_days(Date),
            day_of_week = calendar:day_of_the_week(Date),
            is_unique = from_binary(proplists:get_value(<<"is_unique">>, Proplist, undefined)) > 0,
            ip = proplists:get_value(<<"ip">>, Proplist, undefined),
            http_referer = proplists:get_value(<<"referer">>, Proplist, undefined),
            domain = proplists:get_value(<<"referer_host">>, Proplist, undefined),
            user_agent = proplists:get_value(<<"useragent">>, Proplist, undefined),
            subid = dict:from_list([ { Index, Item } || { Index, Item} <- Subid, Item =/= ''])
        },
        { ok, Click }
    end
  catch
    throw:{error,Reason} ->
      error_logger:error_report({error, Reason, Json}),
      {error, Reason}
  end.

from_binary(Term) when not(is_binary(Term)) -> Term;
from_binary(Term) ->
  try
    erlang:binary_to_integer(Term, utf8)
  catch
    error:badarg ->
      try
        binary_to_existing_atom(Term, utf8)
      catch
        error: _ ->
          Term
      end
  end.

uniq_step(Click) ->
  case Click#click_info.is_unique of
    true -> 1;
    false -> 0
  end.
