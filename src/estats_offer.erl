%% Copyright
-module(estats_offer).

-include("include/offer_info.hrl").

%% API
-export([new/0]).

-spec new() -> offer_info.
new() -> #offer_info{
  count = estats_gen_report:new(),
  affiliates = estats_gen_report:new(),
  subid = estats_gen_report:new(),
  domain = estats_gen_report:new(),
  referer = estats_gen_report:new()
}.

