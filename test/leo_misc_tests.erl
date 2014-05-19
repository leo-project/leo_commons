%%====================================================================
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
     fun get_value_/0,
     fun binary_tokens_/0,
     fun env_table_/0,
     fun any_to_binary_/0
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
    false = leo_misc:node_existence(Node, 3000),
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

binary_tokens_() ->
    ?assertEqual([<<"photo">>,<<"image">>,<<"hawaii-0.jpg">>],
                 leo_misc:binary_tokens(<<"photo/image/hawaii-0.jpg">>, <<"/">>)),
    ?assertEqual([<<"photo">>],
                 leo_misc:binary_tokens(<<"photo/">>, <<"/">>)),
    ?assertEqual([<<"photo">>],
                 leo_misc:binary_tokens(<<"/photo/">>, <<"/">>)),
    ?assertEqual([],
                 leo_misc:binary_tokens(<<"/">>, <<"/">>)),
    ?assertEqual([],
                 leo_misc:binary_tokens(<<>>, <<"/">>)),
    ok.


env_table_() ->
    AppName = ?MODULE,
    Key     = "TEST_KEY",
    Val     = "TEST_VAL",

    ok = leo_misc:init_env(),
    undefined = leo_misc:get_env(AppName, Key),
    ok = leo_misc:set_env(AppName, Key, Val),
    {ok, Val} = leo_misc:get_env(AppName, Key),
    ok = leo_misc:init_env(),
    ok.

any_to_binary_() ->
    Bin = <<"leo_commons">>,
    Tuple = {"leo","commons"},
    TupleBin = term_to_binary(Tuple),
    ?assertEqual(Bin, leo_misc:any_to_binary(Bin)),
    ?assertEqual(Bin, leo_misc:any_to_binary("leo_commons")),
    ?assertEqual(Bin, leo_misc:any_to_binary('leo_commons')),
    ?assertEqual(<<"05678">>, leo_misc:any_to_binary("05678")),
    ?assertEqual(<<"5678">>,  leo_misc:any_to_binary(05678)),
    ?assertEqual(TupleBin,    leo_misc:any_to_binary(Tuple)),
    ok.


-endif.

