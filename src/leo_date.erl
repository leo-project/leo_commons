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
%% @doc leo_date is utilities for date processing.
%% @reference https://github.com/leo-project/leo_commons/blob/master/src/leo_date.erl
%% @end
%%======================================================================
-module(leo_date).

-author('Yosuke Hara').

-export([now/0, clock/0, zone/0,
         date_format/0, date_format/1, date_format/2,
         unixtime/0, unixtime_to_greg_seconds/1,
         greg_seconds_to_unixtime/1
        ]).


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
    {H,S,M} = os:timestamp(),
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
date_format() ->
    Timestamp = os:timestamp(),
    {_,_,MicroSec} = Timestamp,
    GregorianSeconds  = calendar:datetime_to_gregorian_seconds(
                          calendar:now_to_universal_time(Timestamp)),

    {{Year, Month, Date},{Hour,Min,Sec}} =
        calendar:universal_time_to_local_time(
          calendar:gregorian_seconds_to_datetime(GregorianSeconds)),
    io_lib:format("~4..0w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w.~w ~s",
                  [Year, Month, Date, Hour, Min, Sec, MicroSec, zone()]).

%% @doc Format date
%%
-spec(date_format(GregorianSeconds) ->
             string() when GregorianSeconds::pos_integer()).
date_format(GregorianSeconds) ->
    {{Year, Month, Date},{Hour,Min,Sec}} =
        calendar:universal_time_to_local_time(
          calendar:gregorian_seconds_to_datetime(GregorianSeconds)),
    io_lib:format("~4..0w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w ~s",
                  [Year, Month, Date, Hour, Min, Sec, zone()]).

%% @doc Format date
%%
-spec(date_format(Zone, GregorianSeconds) ->
             string() when Zone::'utc',
                           GregorianSeconds::pos_integer()).
date_format('utc', GregorianSeconds) ->
    {{Year, Month, Date},{Hour,Min,Sec}} =
        calendar:gregorian_seconds_to_datetime(GregorianSeconds),
    io_lib:format("~4..0w-~2..0w-~2..0wT~2..0w:~2..0w:~2..0wZ",
                  [Year, Month, Date, Hour, Min, Sec]);
date_format(_, _) ->
    {error, badargs}.


%% @doc Retrieve unixtime
%%
-spec(unixtime() ->
             pos_integer()).
unixtime() ->
    {M, S, _} = os:timestamp(),
    (M * 1000000) + S.

%% @doc Convert data from a unixtime to a gregorian seconds
%%
-spec(unixtime_to_greg_seconds(UnixTime) ->
             pos_integer() when UnixTime :: pos_integer()).
unixtime_to_greg_seconds(UnixTime) ->
    M = leo_math:floor(UnixTime / 1000000),
    S = (UnixTime - M * 1000000),
    calendar:datetime_to_gregorian_seconds(
      calendar:now_to_universal_time({M,S,0})).


%% @doc Convert data from a gregorian seconds to a unixtime
%%
-spec(greg_seconds_to_unixtime(pos_integer()) ->
             pos_integer()).
greg_seconds_to_unixtime(GregorianSeconds) ->
    BaseSeconds =
        calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}}),
    GregorianSeconds - BaseSeconds.
