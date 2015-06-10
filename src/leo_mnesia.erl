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
%% Leo Commons - MNESIA Utils.
%%
%% @doc leo_mnesia is utilities for mnesia operation
%% @reference https://github.com/leo-project/leo_commons/blob/master/src/leo_mnesia.erl
%% @end
%%======================================================================
-module(leo_mnesia).

-author('Yosuke Hara').

-include("leo_commons.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/qlc.hrl").

-export([read/1, write/1, delete/1,
         batch/1, export/2, export/3]).


%% @doc Retrieve a value from mnesia
-spec(read(Fun) ->
             {ok, [any()]} | not_found | {error, any()}
                 when Fun::function()).
read(Fun) ->
    case catch mnesia:activity(transaction, Fun) of
        {_, Cause} ->
            {error, Cause};
        [] ->
            not_found;
        List ->
            {ok, List}
    end.


%% @doc Insert a value into mnesia
-spec(write(Fun) ->
             ok | {error, any()}
                 when Fun::function()).
write(Fun) ->
    case catch mnesia:activity(transaction, Fun) of
        ok ->
            ok;
        {_, Cause} ->
            {error, Cause}
    end.


%% @doc Remove a value from mnesia
-spec(delete(Fun) ->
             ok | {error, any()}
                 when Fun::function()).
delete(Fun) ->
    case catch mnesia:activity(transaction, Fun) of
        ok ->
            ok;
        {_, Cause} ->
            {error, Cause}
    end.


%% @doc Bache processing
-spec(batch(Fun) ->
             ok | {error, any()}
                 when Fun::function()).
batch(Fun) ->
    case catch mnesia:activity(transaction, Fun) of
        ok ->
            ok;
        {_, Cause} ->
            {error, Cause}
    end.


%% @doc Export mnesia's records
%%
-spec(export(FilePath, Table) ->
             ok | {error, any()} when FilePath::string(),
                                      Table::atom()).
export(FilePath, Table) ->
    export(FilePath, Table, ?EXPORT_TYPE_TUPLE).


%% @doc Export mnesia's records
%%
-spec(export(FilePath, Table, ExportType) ->
             ok | {error, any()} when FilePath::string(),
                                      Table::atom(),
                                      ExportType::export_type()).
export(FilePath, Table, ExportType) ->
    %% open a file
    {ok, Handler} = file:open(FilePath, [write, append, binary]),
    Rows = mnesia:table_info(Table, size),

    %% output records
    mnesia:transaction(
      fun() ->
              case catch  mnesia:first(Table) of
                  '$end_of_table' ->
                      ok;
                  {'EXIT', _} ->
                      ok;
                  Key ->
                      Ret = mnesia:read(Table, Key, read),
                      case output(Handler, ExportType, Ret) of
                          ok ->
                              export_1(Rows - 1, Handler, Table, ExportType, Key);
                          Error ->
                              Error
                      end
              end
      end),
    %% close a file
    file:close(Handler),
    ok.

%% @private
export_1(0,_Handler,_Table,_ExportType,_Key) ->
    ok;
export_1(Rows, Handler, Table, ExportType, Key) ->
    case catch mnesia:next(Table, Key) of
        '$end_of_table' ->
            ok;
        {'EXIT', Cause} ->
            {error, Cause};
        Key_1 ->
            Ret = mnesia:read(Table, Key_1, read),
            case output(Handler, ExportType, Ret) of
                ok ->
                    export_1(Rows - 1, Handler, Table, ExportType, Key_1);
                Error ->
                    Error
            end
    end.


%% @doc Append a record
%% @private
output(Handler, ?EXPORT_TYPE_TUPLE, Ret) ->
    lists:foreach(fun(X) -> io:format(Handler, "~p.~n",[X]) end, Ret);
output(_,_,_) ->
    {error, not_support_type}.
