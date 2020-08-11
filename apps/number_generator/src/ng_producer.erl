%%%-------------------------------------------------------------------
%%% @author regikul
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(ng_producer).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-include_lib("common/include/common.hrl").

-record(ng_producer_state, {
    redis :: pid(),
    n :: non_neg_integer(),
    out_queue :: string()
}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    {ok, Redis} = common:start_redis(),
    {ok, N} = application:get_env(n),
    true = N > 2,
    {ok, Queue} = application:get_env(out_queue),
    State = #ng_producer_state{
        redis = Redis,
        n = N - 1,
        out_queue = Queue
    },
    {ok, State}.

handle_call(_Request, _From, State = #ng_producer_state{}) ->
    {reply, ok, State}.

handle_cast(_Request, State = #ng_producer_state{}) ->
    {noreply, State}.

handle_info(_Info, State = #ng_producer_state{}) ->
    {noreply, State}.

terminate(_Reason, _State = #ng_producer_state{}) ->
    ok.

code_change(_OldVsn, State = #ng_producer_state{}, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
