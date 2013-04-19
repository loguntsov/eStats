
-include("include/types.hrl").

-record(report_info, {
  module :: module(),
  path :: string(),
  counters :: undefined | tid(),
  map :: undefined | tid(),
  index :: udefined  | tid(),
  counters_data :: term(), % DETS со счетчиками
  map_data :: term(), % DETS соотвествий хеш -> значение
  index_data :: term() % DETS индекса ключ -> список ключей из map
}).

