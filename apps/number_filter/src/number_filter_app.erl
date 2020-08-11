%%%-------------------------------------------------------------------
%% @doc number_filter public API
%% @end
%%%-------------------------------------------------------------------

-module(number_filter_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    number_filter_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
