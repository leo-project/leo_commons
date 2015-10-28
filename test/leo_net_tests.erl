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
%% Network Util Test
%% @doc
%% @end
%%====================================================================
-module(leo_net_tests).
-author('Wilson Li').

-include("leo_commons.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([client/2,
         client/3]).
-define(TEST_SIZE,  1048576).

%%--------------------------------------------------------------------
%% TEST
%%--------------------------------------------------------------------
-ifdef(EUNIT).
client(Port, Bin) ->
    {ok, Socket} = gen_tcp:connect("localhost", Port, [binary, {packet, 0}]),
    leo_net:chunked_send(gen_tcp, Socket, Bin).

client(Port, Bin, ChunkSize) ->
    {ok, Socket} = gen_tcp:connect("localhost", Port, [binary, {packet, 0}]),
    leo_net:chunked_send(gen_tcp, Socket, Bin, ChunkSize).

chunked_send_test() ->
    ?debugMsg("===== Testing chunked_send ====="),
    Bin = crypto:rand_bytes(?TEST_SIZE),

    {ok, LSocket} = gen_tcp:listen(0, [binary, {packet, 0},
                                       {active, false}]),
    {ok, Port} = inet:port(LSocket),

    ?debugMsg("===== Testing chunked_send (default) ====="),
    spawn_link(?MODULE, client, [Port, Bin]),
    {ok, CSocket} = gen_tcp:accept(LSocket),
    {ok, Bin} = gen_tcp:recv(CSocket, ?TEST_SIZE),

    ?debugMsg("===== Testing chunked_send (1 byte) ====="),
    spawn_link(?MODULE, client, [Port, Bin, 1]),
    {ok, CSocket2} = gen_tcp:accept(LSocket),
    {ok, Bin} = gen_tcp:recv(CSocket2, ?TEST_SIZE),

    ?debugMsg("===== Testing chunked_send (4 MB) ====="),
    spawn_link(?MODULE, client, [Port, Bin, 4194304]),
    {ok, CSocket3} = gen_tcp:accept(LSocket),
    {ok, Bin} = gen_tcp:recv(CSocket3, ?TEST_SIZE),
    ok.

-endif.
