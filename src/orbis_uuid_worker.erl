%%%
%%% Copyright (c) 2015 Alexander Færøy
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%
%%% ----------------------------------------------------------------------------
%%% @author Alexander Færøy <ahf@0x90.dk>
%%% @doc Orbis UUID Worker.
%%% @end
%%% ----------------------------------------------------------------------------
-module(orbis_uuid_worker).
-behaviour(orbis_worker).
-behaviour(gen_server).

%% API.
-export([create/1]).

%% Orbis Worker.
-export([start_link/1]).

%% Generic Server Callbacks.
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {
    partition    :: orbis_chash_bucket:partition(),
    sequence     :: non_neg_integer(),
    shard        :: non_neg_integer(),
    epoch_offset :: non_neg_integer(),
    time_offset  :: non_neg_integer()
}).

-define(SERVER, ?MODULE).

-spec create(Worker :: pid()) -> integer() | {error, term()}.
create(Worker) ->
    gen_server:call(Worker, create).

-spec start_link([term()]) -> {ok, pid()} | ignore | {error, term()}.
start_link(Arguments) ->
    gen_server:start_link(?SERVER, Arguments, []).

%% @private
init([_Name, Partition | _Arguments]) ->
    erlang:monitor(time_offset, clock_service),
    {ok, EpochOffset} = application:get_env(orbis_uuid, epoch_offset),
    io:format("Worker: ~p started with epoch offset: ~p, partition: ~p~n", [self(), EpochOffset, Partition]),
    {ok, #state {
        partition    = Partition,
        sequence     = 0,
        shard        = Partition bsr (256 - 12),
        epoch_offset = EpochOffset,
        time_offset  = erlang:time_offset(milli_seconds)
    }}.

%% @private
handle_call(create, _From, #state { sequence = Sequence, shard = Shard, epoch_offset = EpochOffset, time_offset = TimeOffset } = State) ->
    Timestamp = erlang:monotonic_time(milli_seconds) + TimeOffset - EpochOffset,
    Reply = (Timestamp bsl 22) bor (Shard bsl 10) bor Sequence,
    {reply, Reply, State#state { sequence = (Sequence + 1) rem 1024 }};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%% @private
handle_cast(_Message, State) ->
    {noreply, State}.

%% @private
handle_info({'CHANGE', _Ref, time_offset, clock_service, NativeTimeOffset}, #state { time_offset = CurrentTimeOffset } = State) ->
    TimeOffset = erlang:convert_time_unit(NativeTimeOffset, native, milli_seconds),
    TimeDelta  = TimeOffset - CurrentTimeOffset,
    case TimeDelta of
        TimeDelta when TimeDelta > 0 ->
            io:format("Time changed ~.5f seconds forward: updating time offset~n", [TimeDelta / 1000]),
            {noreply, State#state { time_offset = TimeOffset }};

        TimeDelta when TimeDelta < 0 ->
            io:format("Time changed ~.5f seconds backwards: ignoring~n", [TimeDelta / 1000]),
            {noreply, State};

        _ ->
            {noreply, State}
    end;

handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVersion, State, _Extra) ->
    {ok, State}.
