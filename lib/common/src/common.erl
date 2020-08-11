-module(common).

-include("common.hrl").

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([start_redis/0]).

-export([child/1]).

-spec child(atom()) -> map().
child(Child) ->
    #{id => Child, start => {Child, start_link, []}}.

-spec start_redis() -> either(pid(), atom()).
start_redis() ->
    case application:get_env(redis_url) of
        undefined -> {error, not_configured};
        {ok, Url} ->
            connect_redis(Url)
    end.

-spec splitter(binary()) -> tuple().
splitter(Binary) ->
    case binary:split(Binary, <<"=">>) of
        [Key] -> Key;
        [Key, Value] -> {Key, Value}
    end.

-spec connect_redis(binary()) -> either(pid(), atom()).
connect_redis(Url) ->
    Map = uri_string:parse(Url),
    #{host := Host,
      scheme := Scheme
    } = Map,
    QS = list_to_binary(maps:get(query, Map, "")),
    Port = maps:get(port, Map, 6379),
    Params = case QS =:= <<>> orelse binary:split(QS, <<"&">>, [trim_all, global]) of
                 true -> [];
                 Binaries -> lists:map(fun splitter/1, Binaries)
             end,
    Database = case proplists:get_value(<<"database">>, Params) of
                   undefined -> undefined;
                   Value when is_binary(Value) -> list_to_integer(binary_to_list(Value))
               end,
    Password = case proplists:get_value(<<"password">>, Params) of
                   undefined -> undefined;
                   Pass when is_binary(Pass) -> binary_to_list(Pass)
               end,
    ReconnectSleep = case proplists:get_value(<<"reconnect_sleep">>, Params) of
                         undefined -> 100;
                         <<"no_reconnect">> -> no_reconnect;
                         Sleep when is_binary(Sleep) -> list_to_integer(binary_to_list(Sleep))
                     end,
    ConnectTimeout = case proplists:get_value(<<"connect_timeout">>, Params) of
                         undefined -> 5000;
                         Timeout when is_binary(Timeout) -> list_to_integer(binary_to_list(Timeout))
                     end,
    if
        Scheme =/= "redis" -> {error, bad_schema};
        true ->
            eredis:start_link(Host, Port, Database, Password, ReconnectSleep, ConnectTimeout)
    end.

-ifdef(EUNIT).

-define(BAD_SCHEMA_URL, "http://localhost").
-define(SHORT_URL, "redis://localhost").
-define(FULL_URL, "redis://localhost:6379/?database=0&password=password&reconnect_sleep=100&connect_timeout=5000").
-define(NO_RECONNECT_URL, "redis://localhost:6379/?database=0&password=password&reconnect_sleep=no_reconnect&connect_timeout=5000").
-define(EMPTY_PASSWORD_URL, "redis://localhost:6379/?database=0&password=&reconnect_sleep=100&connect_timeout=5000").

redis_bad_schema_test() ->
    ?assertEqual({error, bad_schema}, connect_redis(?BAD_SCHEMA_URL)).

redis_short_url_test() ->
    meck:new(eredis, [non_strict]),
    meck:expect(eredis, start_link, fun redis_short_link_values/6),
    ?assertMatch({ok, _}, connect_redis(?SHORT_URL)),
    ?assert(meck:validate(eredis)),
    meck:unload(eredis).

redis_full_url_test() ->
    meck:new(eredis, [non_strict]),
    meck:expect(eredis, start_link, fun redis_full_link_values/6),
    ?assertMatch({ok, _}, connect_redis(?FULL_URL)),
    ?assert(meck:validate(eredis)),
    meck:unload(eredis).

redis_no_reconnect_url_test() ->
    meck:new(eredis, [non_strict]),
    meck:expect(eredis, start_link, fun redis_no_reconnect_link_values/6),
    ?assertMatch({ok, _}, connect_redis(?NO_RECONNECT_URL)),
    ?assert(meck:validate(eredis)),
    meck:unload(eredis).

redis_empty_password_url_test() ->
    meck:new(eredis, [non_strict]),
    meck:expect(eredis, start_link, fun redis_empty_password_link_values/6),
    ?assertMatch({ok, _}, connect_redis(?EMPTY_PASSWORD_URL)),
    ?assert(meck:validate(eredis)),
    meck:unload(eredis).

redis_short_link_values("localhost", 6379, undefined, undefined, 100, 5000) ->
    {ok, self()}.

redis_full_link_values("localhost", 6379, 0, "password", 100, 5000) ->
    {ok, self()}.

redis_no_reconnect_link_values("localhost", 6379, 0, "password", no_reconnect, 5000) ->
    {ok, self()}.

redis_empty_password_link_values("localhost", 6379, 0, "", 100, 5000) ->
    {ok, self()}.

-endif.
