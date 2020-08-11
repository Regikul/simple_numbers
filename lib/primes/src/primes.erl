-module(primes).
-author("regikul").

%% API
-export([verify/1]).

-on_load(init/0).

-spec verify(pos_integer()) -> boolean().
verify(_Integer) -> erlang:nif_error(not_loaded).

init() ->
    PrivDir = code:priv_dir(primes),
    SoPath = filename:join(PrivDir, "primes"),
    erlang:load_nif(SoPath, 0).
