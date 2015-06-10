%%======================================================================
%%
%% Leo Commons
%%
%% Copyright (c) 2012-2015 Rakuten, Inc.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% ---------------------------------------------------------------------
%% Leo Commons - Utils
%%
%% @doc leo_math is utilities for calculation
%% @reference https://github.com/leo-project/leo_commons/blob/master/src/leo_math.erl
%% @end
%%======================================================================
-module(leo_math).

-author('Yosuke Hara').
-export([power/2, floor/1, ceiling/1]).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------
%% @doc Calculate power value
%% @end
-spec(power(N, P) ->
             integer() when N::integer(),
                            P::integer()).
power(N, P) when is_integer(N), P == 0 -> 1;
power(N, P) when is_integer(N), P >  0 -> N * power(N,  P - 1);
power(_, _) -> 0.


%% @doc Calculate floor value
%% @end
-spec(floor(N) ->
             integer() when N::number()).
floor(N) when N < 0 ->
    T = trunc(N),
    case N - T == 0 of
        true  -> T;
        false -> T - 1
    end;
floor(N) ->
    trunc(N).


%% @doc Calulate ceiling value
%% @end
-spec(ceiling(N) ->
             integer() when N::number()).
ceiling(N) when N < 0 ->
    trunc(N);
ceiling(N) ->
    T = trunc(N),
    case N - T == 0 of
        true  -> T;
        false -> T + 1
    end.
