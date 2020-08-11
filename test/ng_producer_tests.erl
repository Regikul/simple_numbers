-module(ng_producer_tests).
-author("regikul").

-include_lib("eunit/include/eunit.hrl").

simple_test() ->
    meck:new(eredis, [non_strict]),
    Self = self(),
    meck:expect(eredis, q, fun (_Pid, _List) -> Self ! {add, 1} end),
    meck:new(common),
    meck:expect(common, start_redis, fun() -> {ok, self()} end),
    meck:new(application, [unstick, passthrough]),
    meck:expect(application, get_env, fun get_env_expect/1),
    Since = erlang:monotonic_time(microsecond),
    {ok, Producer} = ng_producer:start_link(),
    timer:sleep(timer:seconds(2)),
    gen_server:stop(Producer),
    Now = erlang:monotonic_time(microsecond),
    MsgCount = count_messages(),
    Elapsed = Now - Since,
    ExpectedMsgCount = 3 * Elapsed div 1000 + (Elapsed rem 1000 div 333),
    ?assert(abs(ExpectedMsgCount - MsgCount) < 10),
    meck:unload().

get_env_expect(n) -> {ok, 1000};
get_env_expect(out_queue) -> {ok, "some_queue"}.

count_messages() ->
    receive
        {add, X} -> X + count_messages()
    after 200 ->
        0
    end.
