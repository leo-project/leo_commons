%%====================================================================
%%
%% Leo Commons
%%
%% Copyright (c) 2012-2014 Rakuten, Inc.
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
%% -------------------------------------------------------------------
%% Mnesia Test
%% @doc
%% @end
%%====================================================================
-module(leo_math_tests).
-author('Yosuke Hara').

-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%% TEST
%%--------------------------------------------------------------------
-ifdef(EUNIT).
math_test_() ->
    [
     fun power_/0,
     fun floor_/0,
     fun ceiling_/0
    ].

power_() ->
    0         = leo_math:power(-1, -1),
    0         = leo_math:power( 0,  1),
    1         = leo_math:power(10,  0),
    10        = leo_math:power(10,  1),
    100       = leo_math:power(10,  2),
    1000      = leo_math:power(10,  3),
    10000     = leo_math:power(10,  4),
    100000    = leo_math:power(10,  5),
    1000000   = leo_math:power(10,  6),
    10000000  = leo_math:power(10,  7),
    100000000 = leo_math:power(10,  8),
    ok.

floor_() ->
    -1 = leo_math:floor(-0.1),
    -1 = leo_math:floor(-0.2),
    -1 = leo_math:floor(-0.3),
    -1 = leo_math:floor(-0.4),
    -1 = leo_math:floor(-0.5),
    -1 = leo_math:floor(-0.6),
    -1 = leo_math:floor(-0.7),
    -1 = leo_math:floor(-0.8),
    -1 = leo_math:floor(-0.9),
    -1 = leo_math:floor(-1),
    0  = leo_math:floor(0),
    1  = leo_math:floor(1),
    1  = leo_math:floor(1.1),
    1  = leo_math:floor(1.2),
    1  = leo_math:floor(1.3),
    1  = leo_math:floor(1.4),
    1  = leo_math:floor(1.5),
    1  = leo_math:floor(1.6),
    1  = leo_math:floor(1.7),
    1  = leo_math:floor(1.8),
    1  = leo_math:floor(1.9),
    2  = leo_math:floor(2),
    ok.

ceiling_() ->
    0  = leo_math:ceiling(-0.1),
    0  = leo_math:ceiling(-0.2),
    0  = leo_math:ceiling(-0.3),
    0  = leo_math:ceiling(-0.4),
    0  = leo_math:ceiling(-0.5),
    0  = leo_math:ceiling(-0.6),
    0  = leo_math:ceiling(-0.7),
    0  = leo_math:ceiling(-0.8),
    0  = leo_math:ceiling(-0.9),
    -1 = leo_math:ceiling(-1),
    0  = leo_math:ceiling(0),
    1  = leo_math:ceiling(1),
    2  = leo_math:ceiling(1.1),
    2  = leo_math:ceiling(1.2),
    2  = leo_math:ceiling(1.3),
    2  = leo_math:ceiling(1.4),
    2  = leo_math:ceiling(1.5),
    2  = leo_math:ceiling(1.6),
    2  = leo_math:ceiling(1.7),
    2  = leo_math:ceiling(1.8),
    2  = leo_math:ceiling(1.9),
    2  = leo_math:ceiling(2),
    ok.

-endif.

