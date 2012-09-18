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
%% Hashtable Test
%% @doc
%% @end
%%====================================================================
-module(leo_hashtable_tests).
-author('Yosuke Hara').

-include("leo_commons.hrl").
-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%% TEST
%%--------------------------------------------------------------------
-ifdef(EUNIT).
hashtable_test_() ->
    [
     fun put_immutable_/0,
     fun put_mutable_/0,
     fun append_/0,
     fun suite_/0
    ].


put_immutable_() ->
    H = leo_hashtable:new(immutable),
    ok = leo_hashtable:put(H,'test',5),
    {error,already_defined} = leo_hashtable:put(H,'test',6),
    Ret = leo_hashtable:get(H,'test'),
    ?assertEqual(5, Ret).

put_mutable_() ->
    H = leo_hashtable:new(),
    ok = leo_hashtable:put(H,'test',5),
    ok = leo_hashtable:put(H,'test',7),
    Ret = leo_hashtable:get(H,'test'),
    ?assertEqual(7, Ret).

append_() ->
    H = leo_hashtable:new(),
    ok = leo_hashtable:append(H,'test-a',1),
    ok = leo_hashtable:append(H,'test-a',2),
    ok = leo_hashtable:append(H,'test-a',3),
    ok = leo_hashtable:append(H,'test-a',4),
    ok = leo_hashtable:append(H,'test-a',5),
    Value= leo_hashtable:get(H, 'test-a'),
    ?assertEqual([1,2,3,4,5], lists:sort(Value)),

    ok = leo_hashtable:put(H,'test-b',1),
    {error, _} = leo_hashtable:append(H,'test-b',1).

suite_() ->
    H = leo_hashtable:new(),
    ok = leo_hashtable:put(H,'test1',3),
    ok = leo_hashtable:put(H,'test2',5),
    ok = leo_hashtable:put(H,'test3',7),

    ok = leo_hashtable:incr(H,'test1'),
    4  = leo_hashtable:get(H,'test1'),

    Ret = leo_hashtable:all(H),
    ?assertEqual(3, erlang:length(Ret)),

    Keys0 = leo_hashtable:keys(H),
    ?assertEqual(3, erlang:length(Keys0)),
    
    _ = leo_hashtable:delete(H, 'test1'),
    Keys1 = leo_hashtable:keys(H),
    ?assertEqual(2, erlang:length(Keys1)),

    _ = leo_hashtable:clear(H),
    Keys2 = leo_hashtable:keys(H),
    ?assertEqual(0, erlang:length(Keys2)),

    _ = leo_hashtable:destroy(H),
    ok.
    

-endif.
