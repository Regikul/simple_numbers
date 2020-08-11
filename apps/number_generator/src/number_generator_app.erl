%%%-------------------------------------------------------------------
%% @doc number_generator public API
%% @end
%%%-------------------------------------------------------------------

-module(number_generator_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    number_generator_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
