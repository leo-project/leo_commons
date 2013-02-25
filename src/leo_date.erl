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
-module(leo_date).

-author('Yosuke Hara').

-export([now/0, clock/0, zone/0, date_format/1, date_format/2]).


%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------
%% @doc Retrieve the current time. (the number of seconds from year 0 to now)
-spec(now() ->
             integer()).
now() ->
    calendar:datetime_to_gregorian_seconds(calendar:universal_time()).


%% @doc Retrieve current time with unix-time.
%%
-spec(clock() ->
             integer()).
clock() ->
    {H,S,M} = erlang:now(),
    1000000000000 * H + (S * 1000000 + M).


%% @doc Retrieve current timezone
%%
-spec(zone() ->
             string()).
zone() ->
    Time = erlang:universaltime(),
    LocalTime = calendar:universal_time_to_local_time(Time),
    DiffSecs = calendar:datetime_to_gregorian_seconds(LocalTime) - calendar:datetime_to_gregorian_seconds(Time),
    zone((DiffSecs/3600)*100).
zone(Val) when Val < 0 ->
    io_lib:format("-~4..0w", [trunc(abs(Val))]);
zone(Val) when Val >= 0 ->
    io_lib:format("+~4..0w", [trunc(abs(Val))]).


%% @doc Format date
%%
-spec(date_format(integer()) ->
             string()).
date_format(GregorianSeconds) ->
    {{Year, Month, Date},{Hour,Min,Sec}} =
        calendar:universal_time_to_local_time(
          calendar:gregorian_seconds_to_datetime(GregorianSeconds)),
    io_lib:format("~4..0w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w ~s",
                  [Year, Month, Date, Hour, Min, Sec, zone()]).

-spec(date_format(type_of_now, integer()) ->
             string()).
date_format(type_of_now, Timestamp) ->
    {_,_,MicroSec} = Timestamp,
    GregorianSeconds  = calendar:datetime_to_gregorian_seconds(
                          calendar:now_to_universal_time(Timestamp)),

    {{Year, Month, Date},{Hour,Min,Sec}} =
        calendar:universal_time_to_local_time(
          calendar:gregorian_seconds_to_datetime(GregorianSeconds)),
    io_lib:format("~4..0w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w.~w ~s",
                  [Year, Month, Date, Hour, Min, Sec, MicroSec, zone()]).
