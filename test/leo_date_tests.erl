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
-module(leo_date_tests).
-author('Yosuke Hara').

-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%% TEST
%%--------------------------------------------------------------------
-ifdef(EUNIT).
date_test_() ->
    [
     fun now_/0,
     fun clock_/0,
     fun zone_/0,
     fun date_format_/0,
     fun unixtime_to_greg_seconds_/0,
     fun greg_seconds_to_unixtime_/0
    ].

now_() ->
    Ret = leo_date:now(),
    ?assertEqual(true, 63515000000 < Ret),
    ok.

clock_() ->
    Ret = leo_date:clock(),
    ?assertEqual(true, 1347900000000000 < Ret),
    ok.

zone_() ->
    Ret = leo_date:zone(),
    ?assertEqual(true, undefined =/= Ret),
    ?assertEqual(true, [] =/= Ret),
    ok.

date_format_() ->
    Ret1 = leo_date:date_format(leo_date:now()),
    ?assertEqual(true, undefined =/= Ret1),
    ?assertEqual(true, [] =/= Ret1),

    Ret2 = leo_date:date_format(),
    ?assertEqual(true, undefined =/= Ret2),
    ?assertEqual(true, [] =/= Ret2),

    Ret3 = leo_date:date_format('utc', leo_date:now()),
    ?assertEqual(true, undefined =/= Ret3),
    ?assertEqual(true, [] =/= Ret3),
    ok.


unixtime_to_greg_seconds_() ->
    UnixTime = leo_date:unixtime(),
    GregSec  = leo_date:unixtime_to_greg_seconds(UnixTime),
    ?assertEqual(true, 0 < GregSec),
    timer:sleep(1000),
    ?assertEqual(true, GregSec =< leo_date:now()),
    ok.

greg_seconds_to_unixtime_() ->
    UnixTime_1 = leo_date:greg_seconds_to_unixtime(leo_date:now()),
    timer:sleep(1000),
    UnixTime_2 = leo_date:unixtime(),
    ?assertEqual(true, 0 < UnixTime_1),
    ?assertEqual(true, UnixTime_1 =< UnixTime_2),
    ok.

-endif.
