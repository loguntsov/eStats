%% Copyright
-module(estats_offer).

-include("include/offer_info.hrl").

%% API
-export([new/0]).

-spec new() -> offer_info.
new() -> #offer_info{
  count = estats_report:new(),
  affiliates = estats_report:new(),
  subid = estats_report:new(),
  domain = estats_report:new(),
  referer = estats_report:new()
}.

