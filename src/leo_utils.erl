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
-module(leo_utils).

-author('Yosuke Hara').
-author('Yoshiyuki Kanno').

-export([power/2, floor/1, ceiling/1, round/2]).
-export([now/0, clock/0, zone/0, date_format/1, date_format/2,
         node_to_part_of_file_name/0, node_existence/1, gen_checksum/1]).

-export([file_unconsult/2, file_touch/1, file_get_mount_path/1,
         file_get_remain_disk/1, file_get_total_size/1, file_delete_all/1]).


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


%% @doc Round value.
%%
-spec(round(integer(), integer()) ->
             integer()).
round(Value, Threshold) ->
    Value - Threshold * (Value div Threshold).


%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------
%% @doc Get the current time. (the number of seconds from year 0 to now)
-spec(now() ->
             integer()).
now() ->
    calendar:datetime_to_gregorian_seconds(calendar:universal_time()).


%% @doc
%%
-spec(clock() ->
             integer()).
clock() ->
    {H,S,M} = erlang:now(),
    1000000000000 * H + (S * 1000000 + M).


%% @doc
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


%% @doc
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


%% @doc
%%
-spec(node_to_part_of_file_name() ->
             string()).
node_to_part_of_file_name() ->
    re:replace(atom_to_list(erlang:node()),"@","_at_",[{return, list}]).


%% @doc check a node existence.
%%
-spec(node_existence(atom()) ->
             true | false).
node_existence(Node) ->
    case net_adm:ping(Node) of
        pong ->
            true;
        pang ->
            false
    end.


%% @doc
%%
-spec(gen_checksum(any()) -> binary()).
gen_checksum(Arg) ->
    CRC32 = erlang:list_to_binary(
              leo_hex:integer_to_hex(erlang:crc32(term_to_binary(Arg)))),
    CRC32.


%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------
%% @doc file unconsult
%% @end
-spec(file_unconsult(string(), any()) ->
             ok | {error, any()}).
file_unconsult(File, L) ->
    try
        {ok, S} = file:open(File, write),
        lists:foreach(fun(X) -> io:format(S, "~p.~n",[X]) end, L),
        file:close(S),
        ok
    catch
        _ ->
            {error, 'error file unconsult'}
    end.


%% @doc
%%
-spec(file_touch(string()) ->
             ok | {error, any}).
file_touch(FilePath) ->
    case file:open(FilePath, [raw, write, binary]) of
        {ok, FileHandler} ->
            file:close(FileHandler);
        Error ->
            Error
    end.


%% @doc
%%
-spec(file_get_mount_path(list(), any()) ->
             ok | {error, any()}).
file_get_mount_path([], _DiskData) ->
    {error, "not avaivale"};
file_get_mount_path([_H|Rest], DiskData) ->
    Dir = lists:foldr(
            fun(A, AccIn) ->
                    case A of
                        "/" ->
                            A;
                        _Other ->
                            AccIn ++ A ++ "/"
                    end
            end, [], Rest),
    TrimDir = case Dir of
                  "/" ->
                      "/";
                  Other ->
                      Len = string:len(Other),
                      string:left(Other, Len - 1)
              end,
    case lists:keyfind(TrimDir, 1, DiskData) of
        false ->
            file_get_mount_path(Rest, DiskData);
        MountPath ->
            {ok, MountPath}
    end.

-spec(file_get_mount_path(string()) ->
             ok | {error, any()}).
file_get_mount_path(FilePath) ->
    case disksup:get_disk_data() of
        [{"none",0,0}] ->
            {error, "not avaivale"};
        DiskData when is_list(DiskData) ->
            Tokens = filename:split(FilePath),
            RevTokens = lists:reverse(Tokens),
            file_get_mount_path(RevTokens, DiskData)
    end.


%% @doc
%%
-spec(file_get_remain_disk({string(), integer(), integer()}) ->
             ok | {error, any()}).
file_get_remain_disk({_Path, SizeKB, Rate}) when is_integer(SizeKB),
                                                 is_integer(Rate),
                                                 Rate >= 0, Rate =< 100 ->
    TotalByte = SizeKB * 1024,
    trunc(TotalByte * (100 - Rate) / 100);
file_get_remain_disk(_Other) ->
    {error, badarg}.


%% @doc
%%
-spec(file_get_total_size(string()) ->
             ok | {error, any()}).
file_get_total_size(Path) ->
    file_get_total_size(Path, 0).
file_get_total_size(Path, Acc) ->
    case filelib:is_dir(Path) of
        true ->
            case file:list_dir(Path) of
                {ok, Files} ->
                    lists:foldl(
                      fun(A, AccIn) ->
                              file_get_total_size(Path ++ "/" ++ A, AccIn)
                      end, Acc, Files);
                Error ->
                    throw(Error)
            end;
        false ->
            case filelib:is_regular(Path) of
                true ->
                    Size = filelib:file_size(Path),
                    Acc + Size;
                false ->
                    Acc
            end
    end.


%% @doc
%%
-spec(file_delete_all(string()) ->
             ok | {error, any()}).
file_delete_all(Path) ->
    case filelib:is_dir(Path) of
        true ->
            case file:list_dir(Path) of
                {ok, Files} ->
                    lists:foreach(
                      fun(A) ->
                              file_delete_all(Path ++ A)
                      end, Files);
                Error ->
                    throw(Error)
            end,
            case file:del_dir(Path) of
                ok ->
                    ok;
                ErrorDel ->
                    throw(ErrorDel)
            end;
        false ->
            case filelib:is_regular(Path) of
                true ->
                    case file:delete(Path) of
                        ok ->
                            ok;
                        Error ->
                            throw(Error)
                    end;
                false ->
                    ok
            end
    end.

