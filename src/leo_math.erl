%%======================================================================
%%
%% Leo Commons
%%
%% Copyright (c) 2012 Rakuten, Inc.
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
%% @doc
%% @end
%%======================================================================
-module(leo_math).

-author('Yosuke Hara').
-export([power/2, floor/1, ceiling/1]).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------
%% @doc
%% @end
-spec(power(integer(), integer()) ->
             integer()).
power(N, P) when is_integer(N), P == 0 -> 1;
power(N, P) when is_integer(N), P >  0 -> N * power(N,  P - 1);
power(_, _) -> 0.


%% @doc floor value.
%% @end
-spec(floor(number()) ->
             integer()).
floor(X) when X < 0 ->
    T = trunc(X),
    case X - T == 0 of
        true  -> T;
        false -> T - 1
    end;
floor(X) ->
    trunc(X).


%% @doc ceiling value.
%% @end
-spec(ceiling(number()) ->
             integer()).
ceiling(X) when X < 0 ->
    trunc(X);
ceiling(X) ->
    T = trunc(X),
    case X - T == 0 of
        true  -> T;
        false -> T + 1
    end.

