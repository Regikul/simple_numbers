-module(nf_filter).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).
-define(NO_CONNECTION_DELAY, 500).

-record(nf_filter_state, {
    redis :: pid(),
    in_queue :: string(),
    out_queue :: string(),
    batch_size :: pos_integer(),
    kick_size :: non_neg_integer(),
    enqueued :: non_neg_integer()
}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    {ok, Redis} = common:start_redis(),
    {ok, InQueue} = application:get_env(in_queue),
    {ok, OutQueue} = application:get_env(out_queue),
    {ok, BatchSize} = application:get_env(batch_size),
    {ok, MinBatchSize} = application:get_env(min_batch_size),
    State = #nf_filter_state{
        redis = Redis,
        in_queue = InQueue,
        out_queue = OutQueue,
        batch_size = BatchSize,
        enqueued = 0,
        kick_size = MinBatchSize
    },
    {ok, kick(State)}.

handle_call(Request, _From, State = #nf_filter_state{}) ->
    lager:error("unknown call ~p", [Request]),
    {reply, no_impl, State}.

handle_cast(Request, State = #nf_filter_state{}) ->
    lager:error("unknown cast ~p", [Request]),
    {noreply, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% handle case when we have no active redis
%% connection. Filter proc will try to save
%% CPU time - some kind of 'ping' mode
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_info({response, {error, Reason}}, State) 
    when Reason =:= tcp_closed orelse Reason =:= no_connection ->
    lager:warning("trying to ping, b/c of ~p", [Reason]),
    NewState = count_accepted(State),
    case NewState#nf_filter_state.enqueued of
        0 ->
            timer:sleep(?NO_CONNECTION_DELAY),
            {noreply, ping(NewState)};
        _Else ->
            {noreply, NewState}
    end;
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% normal case, when connection is OK
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_info({response, {ok, Value}}, State) ->
    NewState = count_accepted(State),
    case Value of
        undefined -> ok;
        [_Queue, Binary] when is_binary(Binary) -> process(Binary, State)
    end,
    {noreply, kick(NewState)};
handle_info({response, {error, Reason}}, State) ->
    lager:error("error: ~p", [Reason]),
    NewState = count_accepted(State),
    {noreply, kick(NewState)};
handle_info(Info, State) ->
    lager:error("unknown info ~p", [Info]),
    {noreply, State}.

terminate(_Reason, _State = #nf_filter_state{redis = Redis}) ->
    eredis:stop(Redis),
    ok.

code_change(_OldVsn, State = #nf_filter_state{}, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

process(Binary, #nf_filter_state{redis = Redis,
                                 out_queue = OutQueue}) ->
    Integer = list_to_integer(binary_to_list(Binary)),
    case primes:verify(Integer) of
        true -> eredis:q_noreply(Redis, ["SADD", OutQueue, Integer]);
        false -> ok
    end.

count_accepted(State = #nf_filter_state{enqueued = Enqueued}) ->
    State#nf_filter_state{enqueued = Enqueued - 1}.

command(Queue) ->
    ["BRPOP", Queue, 1].

ping(State = #nf_filter_state{in_queue = InQueue,
                              redis = Redis}) ->
    eredis:q_async(Redis, command(InQueue)),
    State#nf_filter_state{enqueued = 1}.

kick(State = #nf_filter_state{batch_size = BatchSize,
                              kick_size = KickSize,
                              enqueued = Enqueued,
                              in_queue = InQueue,
                              redis = Redis}) ->
    case Enqueued =< KickSize of
        true ->
            Batch = lists:duplicate(BatchSize - Enqueued, command(InQueue)),
            lists:foreach(fun(Command) -> eredis:q_async(Redis, Command) end, Batch),
            State#nf_filter_state{enqueued = BatchSize};
        false -> State
    end.
