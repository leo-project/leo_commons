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
%% Leo Commons - Miscellaneous
%%
%% @doc leo_misc is miscellaneous utilities
%% @reference https://github.com/leo-project/leo_commons/blob/master/src/leo_misc.erl
%% @end
%%======================================================================
-module(leo_misc).

-author('Yosuke Hara').

-export([node_existence/1, node_existence/2,
         get_value/2, get_value/3,
         binary_tokens/2,
         init_env/0, get_env/2, get_env/3, set_env/3,
         any_to_binary/1
        ]).

-include("leo_commons.hrl").
-include_lib("eunit/include/eunit.hrl").

%% @doc check a node existence.
%%
-spec(node_existence(Node) ->
             Existence::boolean() when Node::atom()).
node_existence(Node) ->
    node_existence(Node, 5000).

%% @doc check a node existence.
%%
-spec(node_existence(Node, Timeout) ->
             Existence::boolean() when Node::atom(),
                                       Timeout::pos_integer()).
node_existence(Node, Timeout) ->
    (Node == rpc:call(Node, erlang, node, [], Timeout)).


%% @doc Retrieve a value from prop-lists
%%
-spec(get_value(Key, Props) ->
             undefined | any() when Key::any(),
                                    Props::[tuple()]).
get_value(Key, Props) ->
    get_value(Key, Props, undefined).

%% @doc Retrieve a value from prop-lists
%%
-spec(get_value(Key, Props, Default) ->
             undefined | any() when Key::any(),
                                    Props::[tuple()],
                                    Default::any()).
get_value(Key, Props, Default) ->
    case lists:keyfind(Key, 1, Props) of
        false ->
            Default;
        {_, Value} ->
            Value
    end.


%% @doc Retrieve tokens from binary-data by delimiter-char
%%
-spec(binary_tokens(Bin, Delimiter) ->
             Tokens::[binary()] when Bin::binary(),
                                     Delimiter::binary()).
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
    case ets:info(?ETS_ENV_TABLE) of
        undefined ->
            case ets:new(?ETS_ENV_TABLE,
                         [named_table, set, public, {read_concurrency, true}]) of
                ?ETS_ENV_TABLE ->
                    ok;
                _ ->
                    erlang:error({error, 'could_not_create_ets_table'})
            end;
        _ ->
            ok
    end.


%% @doc Returns the value of the configuration parameter Par application from ETS
%%
-spec(get_env(AppName, Key) ->
             {ok, any()} | undefined when AppName::atom(),
                                          Key::any()).
get_env(AppName, Key) ->
    get_env(AppName, Key, undefined).

%% @doc Returns the value of the configuration parameter Par application from ETS
%%
-spec(get_env(AppName, Key, Default) ->
             {ok, any()} | undefined when AppName::atom(),
                                          Key::any(),
                                          Default::any()).
get_env(AppName, Key, Default) ->
    case ets:lookup(?ETS_ENV_TABLE, {env, AppName, Key}) of
        [{_, Val}] ->
            {ok, Val};
        _ ->
            Default
    end.


%% @doc Sets the value of the configuration parameter Par for Application to ETS
%%
-spec(set_env(AppName, Key, Val) ->
             ok when AppName::atom(),
                     Key::any(),
                     Val::any()).
set_env(AppName, Key, Val) ->
    _ = ets:insert(?ETS_ENV_TABLE, {{env, AppName, Key}, Val}),
    ok.


%% @doc Convert value from any-type to binary
%%
-spec(any_to_binary(V) ->
             binary() when V::binary()).
any_to_binary(V) when is_binary(V) ->
    V;
any_to_binary(V) when is_atom(V) ->
    list_to_binary(atom_to_list(V));
any_to_binary(V) when is_list(V) ->
    list_to_binary(V);
any_to_binary(V) when is_number(V) ->
    list_to_binary(integer_to_list(V));
any_to_binary(V) ->
    term_to_binary(V).
