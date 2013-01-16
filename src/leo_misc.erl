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
%% Leo Commons - Miscellaneous
%% @doc
%% @end
%%======================================================================
-module(leo_misc).

-author('Yosuke Hara').

-export([node_existence/1, get_value/2, get_value/3,
         binary_tokens/2,
         init_env/0, get_env/2, set_env/3
        ]).

-include("leo_commons.hrl").
-include_lib("eunit/include/eunit.hrl").

%% @doc check a node existence.
%%
-spec(node_existence(atom()) ->
             boolean).
node_existence(Node) ->
    (net_adm:ping(Node) == pong).


%% @doc Retrieve a value from prop-lists
%%
-spec(get_value(any(), list(tuple())) ->
             undefined | any()).
get_value(Key, Props) ->
    get_value(Key, Props, undefined).

get_value(Key, Props, Default) ->
    case lists:keyfind(Key, 1, Props) of
        false ->
            Default;
        {_, Value} ->
            Value
    end.


%% @doc Retrieve tokens from binary-data by delimiter-char
%%
-spec(binary_tokens(binary(), binary()) ->
             list()).
binary_tokens(Bin, Delimiter) ->
    case binary:split(Bin, Delimiter, [global,trim]) of
        [<<>>|Rest] ->
            Rest;
        Tokens ->
            Tokens
    end.


%% @doc Initialize table of env
%%
-spec(init_env() ->
             ok).
init_env() ->
    catch ets:new(?ETS_ENV_TABLE,
                  [named_table, set, public, {read_concurrency, true}]),
    ok.


%% @doc Retrieve
%%
-spec(get_env(atom(), any()) ->
             {ok, any()} | undefined).
get_env(AppName, Key) ->
    case ets:lookup(?ETS_ENV_TABLE, {env, AppName, Key}) of
        [{_, Val}] ->
            {ok, Val};
        _ ->
            undefined
    end.


-spec(set_env(atom(), any(), any()) ->
             ok).
set_env(AppName, Key, Val) ->
    _ = ets:insert(?ETS_ENV_TABLE, {{env, AppName, Key}, Val}),
    ok.

