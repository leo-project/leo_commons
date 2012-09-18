%%====================================================================
%%
%% Leo Commons
%%
%% Copyright (c) 2012
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
%% -------------------------------------------------------------------
%% Mnesia Test
%% @doc
%% @end
%%====================================================================
-module(leo_misc_tests).
-author('Yosuke Hara').

-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%% TEST
%%--------------------------------------------------------------------
-ifdef(EUNIT).
misc_test_() ->
    [
     fun node_existence_/0,
     fun get_value_/0
    ].


node_existence_() ->
    %% case-1
    [] = os:cmd("epmd -daemon"),
    {ok, Hostname} = inet:gethostname(),
    Node = list_to_atom("node_0@" ++ Hostname),
    net_kernel:start([Node, shortnames]),

    true = leo_misc:node_existence(Node),
    net_kernel:stop(),

    %% case-2
    false = leo_misc:node_existence(Node),
    ok.

get_value_() ->
    Props = [{a, 1}, {b, 2}, {c, 3}, {d, 4}, {e, 5}],
    1 = leo_misc:get_value('a', Props),
    2 = leo_misc:get_value('b', Props),
    3 = leo_misc:get_value('c', Props),
    4 = leo_misc:get_value('d', Props),
    5 = leo_misc:get_value('e', Props),
    6 = leo_misc:get_value('f', Props, 6),
    undefined = leo_misc:get_value('f', Props),
    ok.

-endif.

