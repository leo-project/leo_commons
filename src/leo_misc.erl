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
         binary_tokens/2
        ]).

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
    binary_tokens(Bin, Delimiter, []).

binary_tokens(<<>>, _, Acc) ->
    lists:reverse(Acc);
binary_tokens(Bin0, Delimiter, Acc0) ->
    case binary:match(Bin0, [Delimiter]) of
        nomatch ->
            Acc1 = [Bin0 | Acc0],
            binary_tokens(<<>>, Delimiter, Acc1);
        {Pos, _} ->
            DLen = byte_size(Delimiter),
            BLen = byte_size(Bin0) - (Pos + DLen),
            Bin1 = binary:part(Bin0, {Pos + DLen, BLen}),

            Acc1 = case binary:part(Bin0, {0, Pos}) of
                       <<>> -> Acc0;
                       Val  -> [Val | Acc0]
                   end,
            binary_tokens(Bin1, Delimiter, Acc1)
    end.

