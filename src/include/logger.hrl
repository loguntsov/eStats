-ifndef(LOGGER_HRL).
-define(LOGGER_HRL, 1).

-define(INFO(Format, Data), error_logger:info_report(Format, Data)).
-define(INFO_(Format), error_logger:info_report(Format)).

-define(WARN(Format, Data), error_logger:info_report(Format, Data)).
-define(WARN_(Format), error_logger:info_report(Format)).

-define(ERROR(Format, Data), error_logger:error_report(Format, Data)).
-define(ERROR_(Format), error_logger:error_report(Format)).

-endif. % LOGGER_HRL
