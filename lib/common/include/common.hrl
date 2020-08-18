-ifndef(_COMMON_HRL).
-define(_COMMON_HRL, true).

-type maybe(Value) :: Value | undefined.
-type either(Value, Error) :: {ok, Value} | {error, Error}.

-define(NO_CONNECTION_DELAY, 500).

-endif.