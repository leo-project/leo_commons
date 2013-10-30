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
%% Leo Commons - MNESIA Utils.
%% @doc
%% @end
%%======================================================================
-module(leo_mnesia).

-author('Yosuke Hara').

-include("leo_commons.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/qlc.hrl").

-export([read/1, write/1, delete/1,
         export/2, export/3]).


%% @doc Retrieve value from mnesia
read(Fun) ->
    case catch mnesia:activity(transaction, Fun) of
        {_, Cause} ->
            {error, Cause};
        [] ->
            not_found;
        List ->
            {ok, List}
    end.

%% @doc Insert value into mnesia
write(Fun) ->
    case catch mnesia:activity(transaction, Fun) of
        ok ->
            ok;
        {_, Cause} ->
            {error, Cause}
    end.

%% @doc Remove value from mnesia
delete(Fun) ->
    case catch mnesia:activity(transaction, Fun) of
        ok ->
            ok;
        {_, Cause} ->
            {error, Cause}
    end.


%% @doc Export mnesia's records
%%
-spec(export(string(), atom()) ->
             ok | {error, any()}).
export(FilePath, Table) ->
    export(FilePath, Table, ?EXPORT_TYPE_TUPLE).

-spec(export(string(), atom(), export_type()) ->
             ok | {error, any()}).
export(FilePath, Table, Type) ->
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
                      case output(Handler, Type, Ret) of
                          ok ->
                              export_1(Rows - 1, Handler, Table, Type, Key);
                          Error ->
                              Error
                      end
              end
      end),

    %% close a file
    file:close(Handler),
    ok.

%% @private
export_1(0,_Handler,_Table,_Type,_Key) ->
    ok;
export_1(Rows, Handler, Table, Type, Key) ->
    case catch mnesia:next(Table, Key) of
        '$end_of_table' ->
            ok;
        {'EXIT', Cause} ->
            {error, Cause};
        Key_1 ->
            Ret = mnesia:read(Table, Key_1, read),
            case output(Handler, Type, Ret) of
                ok ->
                    export_1(Rows - 1, Handler, Table, Type, Key_1);
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
