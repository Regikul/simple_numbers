-module(common_tests).
-author("regikul").

-include_lib("eunit/include/eunit.hrl").

child_test() ->
    ?assertEqual(#{id => test, start => {test, start_link, []}}, common:child(test)).
