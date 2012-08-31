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

-include_lib("stdlib/include/qlc.hrl").

-export([read/1, write/1, delete/1]).

read(Fun) ->
    case catch mnesia:activity(transaction, Fun) of
        {_, Cause} ->
            {error, Cause};
        [] ->
            not_found;
        List ->
            {ok, List}
    end.

write(Fun) ->
    case catch mnesia:activity(transaction, Fun) of
        ok ->
            ok;
        {_, Cause} ->
            {error, Cause}
    end.


delete(Fun) ->
    case catch mnesia:activity(transaction, Fun) of
        ok ->
            ok;
        {_, Cause} ->
            {error, Cause}
    end.

