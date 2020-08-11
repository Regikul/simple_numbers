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
-define(NOW, erlang:monotonic_time(microsecond)).
-define(TICK, 333).
-define(XTICK, 334).

-include_lib("common/include/common.hrl").

-record(ng_producer_state, {
    redis :: pid(),
    n :: non_neg_integer(),
    out_queue :: string(),
    since :: integer()
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
        out_queue = Queue,
        since = ?NOW
    },
    tick(0),
    {ok, State}.

handle_call(_Request, _From, State = #ng_producer_state{}) ->
    {reply, ok, State}.

handle_cast(_Request, State = #ng_producer_state{}) ->
    {noreply, State}.

handle_info({tick, T}, State = #ng_producer_state{since = Since}) ->
    Now = ?NOW,
    TimeDiff = case T =:= 2 of
                   true -> ?XTICK;
                   false -> ?TICK
               end,
    case Now - Since > TimeDiff of
        true ->
            push_number(State),
            tick(T + 1),
            {noreply, State#ng_producer_state{since = Since + TimeDiff}};
        false ->
            tick(T),
            {noreply, State}
    end.

terminate(_Reason, _State = #ng_producer_state{}) ->
    ok.

code_change(_OldVsn, State = #ng_producer_state{}, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

tick(X) -> self() ! {tick, X rem 3}.

push_number(#ng_producer_state{redis = Redis,
                               n = N,
                               out_queue = Queue}) ->
    X = random:uniform(N) + 1,
    eredis:q(Redis, ["LPUSH", Queue, X]).