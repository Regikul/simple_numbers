-module(primes_tests).
-author("regikul").

-include_lib("eunit/include/eunit.hrl").

simplicity_test_() -> [
    ?_assertException(error, badarg, primes:verify(-1)),
    ?_assertException(error, badarg, primes:verify("hello!")),
    ?_assert(not primes:verify(0)),
    ?_assert(not primes:verify(1)),
    ?_assert(primes:verify(2)),
    ?_assert(primes:verify(3)),
    ?_assert(not primes:verify(6)),
    ?_assert(primes:verify(7)),
    ?_assert(not primes:verify(8))
].