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
%% Leo Commons - File Utils
%%
%% @doc leo_file is utilities for file processing
%% @reference https://github.com/leo-project/leo_commons/blob/master/src/leo_file.erl
%% @end
%%======================================================================
-module(leo_file).

-author('Yosuke Hara').
-author('Yoshiyuki Kanno').

-include("leo_commons.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([file_unconsult/2, file_touch/1, file_get_mount_path/1,
         file_get_remain_disk/1, file_get_total_size/1, file_delete_all/1,
         dsize/1,
         pread/3, pread/4
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
            AbsFilePath = filename:absname(FilePath),
            Tokens = filename:split(AbsFilePath),
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


%% @doc Erlang file:pread/3's wrapper function
%%      <http://www.erlang.org/doc/man/file.html#pread-3>
-spec(pread(IoDevice, Location, Number) ->
             {ok, Data} | eof | {error, Reason} when IoDevice::file:io_device(),
                                                     Data::string() | binary(),
                                                     Location::file:location(),
                                                     Number::non_neg_integer(),
                                                     Reason::any() | badarg | terminated).
pread(IoDevice, Location, Number) ->
    pread(IoDevice, Location, Number, ?PREAD_TIMEOUT).

-spec(pread(IoDevice, Location, Number, Timeout) ->
             {ok, Data} | eof | {error, Reason} when IoDevice::file:io_device(),
                                                     Data::string() | binary(),
                                                     Location::file:location(),
                                                     Number::non_neg_integer(),
                                                     Timeout::pos_integer(),
                                                     Reason::any() | badarg | terminated).
pread(IoDevice, Location, Number, Timeout) ->
    case pread_1(IoDevice, Location, Number, <<>>,
                 leo_date:clock(), Timeout, [], 0) of
        {ok, {Bin, []}} ->
            {ok, Bin};
        {ok, {Bin, Errors}} ->
            error_logger:error_msg("~p,~p,~p,~p~n",
                                   [{module, ?MODULE_STRING},
                                    {function, "pread/4"},
                                    {line, ?LINE},
                                    {body, [{errors, lists:reverse(Errors)},
                                            {result, ok}
                                           ]}]),
            {ok, Bin};
        eof ->
            eof;
        {error, {Cause, []}} ->
            {error, Cause};
        {error, {Cause, Errors}} ->
            error_logger:error_msg("~p,~p,~p,~p~n",
                                   [{module, ?MODULE_STRING},
                                    {function, "pread/4"},
                                    {line, ?LINE},
                                    {body, [{errors, lists:reverse(Errors)},
                                            {result, Cause}
                                           ]}]),
            {error, Cause}
    end.


%% @private
-spec(pread_1(IoDevice, Location, Number, Acc,
              StartTime, Timeout, Errors, RetryTimes) ->
             {ok, Data} | eof | {error, Reason} when IoDevice::file:io_device(),
                                                     Data::string() | binary(),
                                                     Location::file:location(),
                                                     Number::non_neg_integer(),
                                                     Acc::binary(),
                                                     StartTime::non_neg_integer(),
                                                     Timeout::pos_integer(),
                                                     Errors::[],
                                                     RetryTimes::non_neg_integer(),
                                                     Reason::term()).
pread_1(IoDevice, Location, Number, Acc,
        StartTime, Timeout, Errors, RetryTimes) ->
    IsTimeout = case (RetryTimes == 0) of
                    true ->
                        false;
                    false ->
                        CurrentTime = leo_date:clock(),
                        Duration = erlang:round((CurrentTime - StartTime) / 1000),
                        (Duration >= Timeout)
                end,
    case file:pread(IoDevice, Location, Number) of
        {ok, DataL} ->
            case byte_size(DataL) of
                Number ->
                    {ok, {<< Acc/binary, DataL/binary >>, Errors}};
                Len when IsTimeout == true ->
                    Errors_1 = [ [{location, Location},
                                  {expected_len, Number},
                                  {actual_len, Len},
                                  {diff, Number - Len}
                                 ] |Errors],
                    {error, {unexpected_len, Errors_1}};
                Len ->
                    timer:sleep(1),
                    pread_1(IoDevice, Location + Len,
                            Number - Len, << Acc/binary, DataL/binary >>,
                            StartTime, Timeout,
                            [ [{location, Location},
                               {expected_len, Number},
                               {actual_len, Len},
                               {diff, Number - Len}
                              ] |Errors], RetryTimes + 1)
            end;
        eof when RetryTimes == 0 ->
            eof;
        eof ->
            {error, {unexpected_len_and_eof, Errors}};
        {error, Reason} ->
            {error, {Reason, Errors}}
    end.
