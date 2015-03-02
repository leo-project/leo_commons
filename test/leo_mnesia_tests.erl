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
%% -------------------------------------------------------------------
%% Mnesia Test
%% @doc
%% @end
%%====================================================================
-module(leo_mnesia_tests).
-author('Yosuke Hara').

-include("leo_commons.hrl").
-include_lib("stdlib/include/qlc.hrl").
-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%% TEST
%%--------------------------------------------------------------------
-ifdef(EUNIT).

-record(test_table, {id   :: integer(),
                     text :: string()
                    }).

mnesia_test_() ->
    [
     fun suite_/0
    ].

suite_() ->
    application:start(mnesia),
    _ = mnesia:create_table(test_table,
                            [{ram_copies, [node()]},
                             {type, set},
                             {record_name, test_table},
                             {attributes, record_info(fields, test_table)},
                             {user_properties,
                              [{id,   {integer, undefined}, false, undefined, undefined, undefined, integer},
                               {text, {varchar, undefined}, false, undefined, undefined, undefined, varchar}
                              ]}
                            ]),

    %% WRITE
    Val0 = #test_table{id = 1, text = "test0"},
    F1 = fun()->
                 mnesia:write(test_table, Val0, write)
         end,
    ok = leo_mnesia:write(F1),

    %% READ-1
    F2 = fun() ->
                Q1 = qlc:q([X || X <- mnesia:table(test_table)]),
                Q2 = qlc:sort(Q1, [{order, descending}]),
                qlc:e(Q2)
        end,
    {ok, [Res1|_]} = leo_mnesia:read(F2),
    ?assertEqual(Val0, Res1),

    F3 = fun() ->
                Q1 = qlc:q([X || X <- mnesia:table(test_table),
                                 X#test_table.id =:= 3]),
                Q2 = qlc:sort(Q1, [{order, descending}]),
                qlc:e(Q2)
        end,
    not_found = leo_mnesia:read(F3),

    %% DELETE
    F4 = fun() ->
                mnesia:delete_object(test_table, Val0, write)
        end,
    ok = leo_mnesia:delete(F4),

    %% READ-2
    F5 = fun() ->
                Q1 = qlc:q([X || X <- mnesia:table(test_table)]),
                Q2 = qlc:sort(Q1, [{order, descending}]),
                qlc:e(Q2)
        end,
    not_found = leo_mnesia:read(F5),

    %% Batch
    F6 = fun() ->
                 Ret_1 = mnesia:write(test_table, Val0, write),
                 Ret_2 = mnesia:delete_object(test_table, Val0, write),
                 ?assertEqual(ok, Ret_1),
                 ?assertEqual(ok, Ret_2),
                 ok
        end,
    ok = leo_mnesia:batch(F6),
    ok.

-endif.
