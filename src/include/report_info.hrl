
-include("include/types.hrl").

-record(report_info, {
  path :: string(),
  counters :: tid(), % ETS со счетчиками
  map :: tid(), % ETS соотвествий хеш -> значение
  index :: tid() % ETS индекса ключ -> список ключей из map
}).

