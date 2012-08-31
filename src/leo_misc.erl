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

-export([node_existence/1]).

%% @doc check a node existence.
%%
-spec(node_existence(atom()) ->
             boolean).
node_existence(Node) ->
    (net_adm:ping(Node) == pong).


%% %% @doc
%% %%
%% -spec(node_to_part_of_file_name() ->
%%              string()).
%% node_to_part_of_file_name() ->
%%     re:replace(atom_to_list(erlang:node()),"@","_at_",[{return, list}]).

%% %% @doc
%% %%
%% -spec(gen_checksum(any()) -> binary()).
%% gen_checksum(Arg) ->
%%     CRC32 = erlang:list_to_binary(
%%               leo_hex:integer_to_hex(erlang:crc32(term_to_binary(Arg)))),
%%     CRC32.


