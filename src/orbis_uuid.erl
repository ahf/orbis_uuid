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
%%% @doc Orbis UUID API.
%%% @end
%%% ----------------------------------------------------------------------------
-module(orbis_uuid).

%% API.
-export([create/0]).

-spec create() -> integer().
create() ->
    dispatch(rand:uniform(), fun (Worker) ->
                                 orbis_uuid_worker:create(Worker)
                             end).

%% @private
-spec dispatch(Key, Fun) -> term()
    when
        Key :: term(),
        Fun :: fun((Worker :: pid()) -> term()).
dispatch(Key, Fun) ->
    orbis:dispatch(orbis_uuid_pool, Key, Fun).
