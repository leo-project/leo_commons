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
%% Leo Commons - Hex (Util)
%% @doc
%% @end
%%======================================================================
-module(leo_hex).

-author('Yosuke Hara').

-include_lib("eunit/include/eunit.hrl").

-export([binary_to_hex/1,
         byte_to_hex/1,
         integer_to_hex/1,
         hex_to_integer/1,
         hex_to_string/1]).

binary_to_hex(Binary) when is_binary (Binary) ->
    lists:flatten (lists:map (fun byte_to_hex/1, binary_to_list (Binary))).

byte_to_hex(I) when I >= 0, I =< 255 ->
    case length (HexByte = integer_to_hex (I)) of
        2 -> HexByte;
        1 -> [$0 | HexByte]
    end.

integer_to_hex(I) when I <  10 -> integer_to_list (I);
integer_to_hex(I) when I <  16 -> [I - 10 + $a];
integer_to_hex(I) when I >= 16 -> N = I div 16, integer_to_hex (N) ++ integer_to_hex (I rem 16).

hex_to_integer(Hex) ->
    lists:foldl (fun (E, Acc) -> Acc * 16 + dehex (E) end, 0, Hex).

hex_to_string(Hex) ->
    {String, _} = lists:foldr (fun (E, {Acc, nolow}) ->
                                       {Acc, dehex (E)};
                                   (E, {Acc, LO})  ->
                                       {[dehex (E) * 16 + LO | Acc], nolow} end, {[], nolow}, Hex),
    String.

dehex(H) when H >= $a, H =< $f -> H - $a + 10;
dehex(H) when H >= $A, H =< $F -> H - $A + 10;
dehex(H) when H >= $0, H =< $9 -> H - $0.


%%======================================================================
%% TEST
%%======================================================================
binary_to_hex_test() ->
    ?assertEqual("ff", leo_hex:binary_to_hex(<<255>>)),
    ok.

byte_to_hex_test() ->
    ?assertEqual("80", leo_hex:byte_to_hex(128)),
    ok.

integer_to_hex_test() ->
    ?assertEqual("80", leo_hex:integer_to_hex(128)),
    ok.

hex_to_integer_test() ->
    ?assertEqual(703503, leo_hex:hex_to_integer("abc0f")),
    ok.

hex_to_string_test() ->
    leo_hex:hex_to_string(leo_hex:binary_to_hex(<<255>>)),
    ok.


