%%======================================================================
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
%% ---------------------------------------------------------------------
%% Leo Commons - Utils
%%
%% @doc leo_file is utilities for file processing
%% @reference https://github.com/leo-project/leo_commons/blob/master/src/leo_file.erl
%% @end
%%======================================================================
-module(leo_file).

-author('Yosuke Hara').
-author('Yoshiyuki Kanno').

-include_lib("eunit/include/eunit.hrl").

-export([file_unconsult/2, file_touch/1, file_get_mount_path/1,
         file_get_remain_disk/1, file_get_total_size/1, file_delete_all/1,
         dsize/1,
         pread/3, pread/4, pread/5
        ]).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------
%% @doc Unconsult the file
%%
-spec(file_unconsult(FilePath, Term) ->
             ok | {error, any()} when FilePath ::string(),
                                      Term::any()).
file_unconsult(FilePath, Term) ->
    try
        {ok, S} = file:open(FilePath, [write]),
        lists:foreach(fun(X) -> io:format(S, "~p.~n",[X]) end, Term),
        file:close(S),
        ok
    catch
        _ ->
            {error, 'error file unconsult'}
    end.


%% @doc Touch a file
%%
-spec(file_touch(FilePath) ->
             ok | {error, any} when FilePath::string()).
file_touch(FilePath) ->
    case file:open(FilePath, [raw, write, binary]) of
        {ok, FileHandler} ->
            file:close(FileHandler);
        Error ->
            Error
    end.


%% @doc Retrieve file mount path(s)
%%
-spec(file_get_mount_path(List, DiskData) ->
             ok | {error, any()} when List::[string()],
                                      DiskData::any()).
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

%% @doc Retrieve file mount path(s)
%%
-spec(file_get_mount_path(FilePath) ->
             ok | {error, any()} when FilePath::string()).
file_get_mount_path(FilePath) ->
    case disksup:get_disk_data() of
        [{"none",0,0}] ->
            {error, "not avaivale"};
        DiskData when is_list(DiskData) ->
            Tokens = filename:split(FilePath),
            RevTokens = lists:reverse(Tokens),
            file_get_mount_path(RevTokens, DiskData)
    end.


%% @doc Retrieve remain disk capacity of the target
%%
-spec(file_get_remain_disk(Params) ->
             ok | {error, any()} when Params::{FilePath::string(),
                                               SizeKB::non_neg_integer(),
                                               Rate::non_neg_integer()}).
file_get_remain_disk({_Path, SizeKB, Rate}) when is_integer(SizeKB),
                                                 is_integer(Rate),
                                                 Rate >= 0, Rate =< 100 ->
    TotalByte = SizeKB * 1024,
    trunc(TotalByte * (100 - Rate) / 100);
file_get_remain_disk(_Other) ->
    {error, badarg}.


%% @doc Retrieve total of the file size
%%
-spec(file_get_total_size(Path) ->
             ok | {error, any()} when Path::string()).
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


%% @doc Remove all files of the target
%%
-spec(file_delete_all(FilePath) ->
             ok | {error, any()} when FilePath::string()).
file_delete_all(FilePath) ->
    case filelib:is_dir(FilePath) of
        true ->
            case file:list_dir(FilePath) of
                {ok, Files} ->
                    lists:foreach(
                      fun(A) ->
                              file_delete_all(filename:join([FilePath, A]))
                      end, Files);
                Error ->
                    throw(Error)
            end,
            case file:del_dir(FilePath) of
                ok ->
                    ok;
                ErrorDel ->
                    throw(ErrorDel)
            end;
        false ->
            case filelib:is_regular(FilePath) of
                true ->
                    case file:delete(FilePath) of
                        ok ->
                            ok;
                        Error ->
                            throw(Error)
                    end;
                false ->
                    ok
            end
    end.


%% @doc Retrieve data-size w/unit.
%% @private
-define(FILE_KB,       1024).
-define(FILE_MB,    1048586).
-define(FILE_GB, 1073741824).

-spec(dsize(integer()) ->
             string()).
dsize(Size) when Size =< ?FILE_KB -> integer_to_list(Size) ++ "B";
dsize(Size) when Size  > ?FILE_KB -> integer_to_list(erlang:round(Size / ?FILE_KB)) ++ "K";
dsize(Size) when Size  > ?FILE_MB -> integer_to_list(erlang:round(Size / ?FILE_MB)) ++ "M";
dsize(Size) when Size  > ?FILE_GB -> integer_to_list(erlang:round(Size / ?FILE_GB)) ++ "G".


-define(DEF_PREAD_RETRY_TIMES, 3).

%% @doc Erlang file:pread/3's wrapper function
%%      <http://www.erlang.org/doc/man/file.html#pread-3>
-spec(pread(IoDevice, Location, Number) ->
             {ok, Data} | eof | {error, Reason} when IoDevice::file:io_device(),
                                                     Data::string() | binary(),
                                                     Location::file:location(),
                                                     Number::non_neg_integer(),
                                                     Reason::any() | badarg | terminated).
pread(IoDevice, Location, Number) ->
    pread(IoDevice, Location, Number, ?DEF_PREAD_RETRY_TIMES).

-spec(pread(IoDevice, Location, Number, RetryTimes) ->
             {ok, Data} | eof | {error, Reason} when IoDevice::file:io_device(),
                                                     Data::string() | binary(),
                                                     Location::file:location(),
                                                     Number::non_neg_integer(),
                                                     Reason::any() | badarg | terminated,
                                                     RetryTimes::non_neg_integer()).
pread(IoDevice, Location, Number, RetryTimes) ->
    pread(IoDevice, Location, Number, false, RetryTimes).

-spec(pread(IoDevice, Location, Number, IsStrictCheck, RetryTimes) ->
             {ok, Data} | eof | {error, Reason} when IoDevice::file:io_device(),
                                                     Data::string() | binary(),
                                                     Location::file:location(),
                                                     Number::non_neg_integer(),
                                                     IsStrictCheck::boolean(),
                                                     Reason::any() | badarg | terminated,
                                                     RetryTimes::non_neg_integer()).
pread(IoDevice, Location, Number, IsStrictCheck, RetryTimes) ->
    case IsStrictCheck of
        true ->
            pread_1(IoDevice, Location, Number, <<>>, RetryTimes, RetryTimes);
        false ->
            pread_2(IoDevice, Location, Number, [], RetryTimes)
    end.

%% @private
pread_1(_IoDevice,_Location, Number, Acc,_, 0) ->
    case byte_size(Acc) of
        Number ->
            {ok, Acc};
        _Len ->
            {error, unexpected_len}
    end;
pread_1(IoDevice, Location, Number, Acc, TotalRetryTimes, RetryTimes) ->
    case file:pread(IoDevice, Location, Number) of
        {ok, DataL} = Ret ->
            case byte_size(DataL) of
                Number ->
                    Ret;
                Len ->
                    pread_1(IoDevice, Location + Len,
                            Number - Len, << Acc/binary, DataL/binary >>,
                            TotalRetryTimes, RetryTimes - 1)
            end;
        eof when RetryTimes == TotalRetryTimes ->
            eof;
        eof ->
            {error, unexpected_len};
        {error,_Reason} ->
            timer:sleep(100),
            pread_1(IoDevice, Location, Number, <<>>, TotalRetryTimes, RetryTimes - 1)
    end.

%% @private
pread_2(_IoDevice,_Location,_Number, Errors, 0) ->
    {error, Errors};
pread_2(IoDevice, Location, Number, Errors, RetryTimes) ->
    case file:pread(IoDevice, Location, Number) of
        {ok,_DataL} = Ret ->
            Ret;
        eof = Ret ->
            Ret;
        {error, Reason} ->
            timer:sleep(100),
            pread_2(IoDevice, Location, Number,
                    [Reason|Errors], RetryTimes - 1)
    end.


%%======================================================================
%% TEST
%%======================================================================
pread_test() ->
    File = "pread_test.log",
    {ok, IoDevice} = file:open(File, [raw, read, write, binary, append]),
    ok = file:pwrite(IoDevice, 0, crypto:rand_bytes(128)),

    {ok,_Bin_1} = ?MODULE:pread(IoDevice, 1, 32),
    {ok,_Bin_2} = ?MODULE:pread(IoDevice, 64, 64),
    {ok,_Bin_3} = ?MODULE:pread(IoDevice, 1,  64, true, 5),

    {error, unexpected_len} = ?MODULE:pread(IoDevice,  96,  64, true, 5),
    eof = ?MODULE:pread(IoDevice, 129,  32, true, 5),

    ok = file:close(IoDevice),
    ok.
