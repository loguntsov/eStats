
-include("include/types.hrl").

-record(report_info, {
  path :: string(),
  counters = none :: tid(),
  map = none :: tid(),
  index = none :: tid(),
  counters_data :: term(), % DETS со счетчиками
  map_data :: term(), % DETS соотвествий хеш -> значение
  index_data :: term() % DETS индекса ключ -> список ключей из map
}).

