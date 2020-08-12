simple_numbers
=====

An OTP application

Deps
-----

GNU make, C compiler and nif libraries required.

Build
-----

    $ rebar3 compile

Test
-----

    $ rebar3 as test eunit

Assemble releases
-----

Project is divided into 3 releases:
- `allinone` - includes both generator and filter functionality
- `generator` - includes just generator
- `filter` - includes just filter for prime numbers

To assemble release run:

    $ rebar3 release -n $RELEASE_NAME

Run
-----

    $ _build/default/rel/allinone/bin/allinone start

or

    $ _build/default/rel/filter/bin/filter start
    $ _build/default/rel/generator/bin/generator start

Configuration
-----

Configuration file exists at `config/sys.config`.
The keys are:
* `n` - the upper limit of the range 2..N, should satisfy `N > 2`
* `redis_url` - a url to describe connection to Redis, examples are self-explanatory
* `out_queue` - a Redis queue to produce values
* `in_queue` - a Redis queue to fetch values
* `batch_size` - the length of the queue of `eredis:q_async` requests
* `min_batch_size` - used to decide when to issue additions `eredis:q_async` requests
