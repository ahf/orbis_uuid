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
%%% @doc Orbis UUID Utilities.
%%% @end
%%% ----------------------------------------------------------------------------
-module(orbis_uuid_utilities).

%% API.
-export([decode/1]).

-spec decode(UUID :: integer()) -> map().
decode(UUID) when is_integer(UUID) ->
    {ok, EpochOffset} = application:get_env(orbis_uuid, epoch_offset),
    case binary:encode_unsigned(UUID) of
        <<Timestamp:42/integer, Shard:12/integer, Sequence:10/integer>> ->
            {ok, #{ timestamp => Timestamp + EpochOffset, shard => Shard, sequence  => Sequence }};
        _ ->
            {error, invalid_uuid}
    end.
