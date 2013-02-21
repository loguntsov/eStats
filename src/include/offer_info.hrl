
-include("include/report_info.hrl").

-record(offer_info, {
  count :: report_info,
  affiliates :: report_info,
  subid :: report_info,
  domain :: report_info,
  referer :: report_info
}).

