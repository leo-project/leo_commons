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
%% Leo Commons - Network (Util)
%%
%% @doc leo_net is utilities for network transport
%% @reference https://github.com/leo-project/leo_commons/blob/master/src/leo_net.erl
%% @end
%%======================================================================
-module(leo_net).

-author('Wilson Li').

-include_lib("eunit/include/eunit.hrl").

-export([chunked_send/3, chunked_send/4]).

-define(DEF_SENDING_CHUNK_SIZE, 131072). %% 128KB


%% @doc Send chunked objects
%%
-spec(chunked_send(Transport, Socket, IOList) ->
             ok | {error, any()} when Transport::module(),
                                      Socket::inet:socket(),
                                      IOList::iodata()).
chunked_send(Transport, Socket, IOData) ->
    chunked_send(Transport, Socket, IOData, ?DEF_SENDING_CHUNK_SIZE).

-spec(chunked_send(Transport, Socket, IOList, ChunkSize) ->
             ok | {error, any()} when Transport::module(),
                                      Socket::inet:socket(),
                                      IOList::iodata(),
                                      ChunkSize::pos_integer()).
chunked_send(Transport, Socket, IOList, ChunkSize) when is_list(IOList) ->
    Bin = iolist_to_binary(IOList),
    chunked_send(Transport, Socket, Bin, ChunkSize);
chunked_send(Transport, Socket, Bin, ChunkSize) when byte_size(Bin) =< ChunkSize ->
    Transport:send(Socket, Bin);
chunked_send(Transport, Socket, Bin, ChunkSize) ->
    << Head:ChunkSize/binary, Rest/binary >> = Bin,
    case Transport:send(Socket, Head) of
        ok ->
            chunked_send(Transport, Socket, Rest, ChunkSize);
        {error, Cause} ->
            {error, Cause}
    end.
