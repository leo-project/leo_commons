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
%% @reference [https://github.com/leo-project/leo_commons/blob/master/src/leo_file.erl]
%% @end
%%======================================================================
-module(leo_file).

-author('Yosuke Hara').
-author('Yoshiyuki Kanno').

-export([file_unconsult/2, file_touch/1, file_get_mount_path/1,
         file_get_remain_disk/1, file_get_total_size/1, file_delete_all/1,
         dsize/1
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
