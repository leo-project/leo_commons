%%======================================================================
%%
%% Leo Commons / LeoSSEC Test
%%
%% Copyright (c) 2012-2018 Rakuten, Inc.
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
%%======================================================================
-module(leo_ssec_tests).
-author("kunal.tyagi").

-include("leo_commons.hrl").
-include_lib("eunit/include/eunit.hrl").

-ifdef(EUNIT).

%% @doc wrapper to iterate and check from Max to Min (inclusive)
%% @private
check_wrapper(Fn, MinValue, MaxValue) when MaxValue >= MinValue ->
    Fn(MaxValue),
    check_wrapper(Fn, MinValue, MaxValue - 1);
check_wrapper(_Fn, _MinValue, _MaxValue) ->
    true.

-spec(run_test_() -> boolean()).
run_test_() ->
    {setup,
     fun()  -> ok end,
     fun(_) -> ok end,
     [
      {"test gen_salt/1 for input 1...100",
       {timeout, timer:seconds(3),
        fun() -> check_wrapper(fun check_salt/1, 1, 100) end}},
      {"test gen_hash/1 for salt size 32...64, key 64",
       {timeout, timer:seconds(3),
        fun() -> check_wrapper(fun check_hash/1, 32, 64) end}},
      {"test verify_key/1 for salt size 32...64, key 64",
       {timeout, timer:seconds(3),
        fun() -> check_wrapper(fun verify_key/1, 32, 64) end}},
      {"test verify_ssec_algorithm/1",
       {timeout, timer:seconds(3), fun test_verify_ssec_algorithm/0}},
      {"test verify_ssec_key/1",
       {timeout, timer:seconds(3), fun test_verify_ssec_key/0}},
      {"test encrypt_object/2 and decrypt_object/4",
       {timeout, timer:seconds(3),
        fun() -> check_wrapper(fun check_en_de/1, 1, 256) end}}
     ]}.

%% Test 1
check_salt(Len) when Len >= 32, Len =< 64 ->
    ?assertMatch({ok, _}, leo_ssec:gen_salt(Len)),
    ?assertNotEqual(leo_ssec:gen_salt(Len), leo_ssec:gen_salt(Len));
check_salt(Len) ->
    ?assertMatch({error, _}, leo_ssec:gen_salt(Len)).


%% Test 2
check_hash(Len) ->
    AlgoList = [md5, sha, sha256],
    {ok, Key} = leo_ssec:gen_salt(64),
    {ok, Salt1} = leo_ssec:gen_salt(Len),
    {ok, Salt2} = leo_ssec:gen_salt(Len),
    %% Check Hash algo actually generates hash
    lists:foreach(fun(Algo) ->
                          ?assertEqual({Algo, crypto:hmac(Algo, Key, Salt1)},
                                       leo_ssec:gen_hash(Algo, Salt1, Key))
                  end, AlgoList),
    %% Check Hash is diff for diff salts
    HashList = lists:map(fun(Algo) ->
                                 {leo_ssec:gen_hash(Algo, Key, Salt1),
                                  leo_ssec:gen_hash(Algo, Key, Salt2)}
                         end, AlgoList),
    lists:foreach(fun({Hash1, Hash2}) ->
                          ?assertNotEqual(Hash1, Hash2)
                  end, HashList).


%% Test 3
verify_key(Len) ->
    AlgoList = [md5, sha, sha256],
    {ok, Key} = leo_ssec:gen_salt(64),
    {ok, Salt} = leo_ssec:gen_salt(Len),
    HashList = lists:map(fun(Algo) -> leo_ssec:gen_hash(Algo, Key, Salt) end, AlgoList),
    lists:foreach(fun(Hash) ->
                          ?assert(leo_ssec:verify_key(Key, Salt, Hash))
                  end, HashList).


%% Test 4
-spec(verify_block_encryption_test_() -> boolean()).
verify_block_encryption_test_() ->
    [
     fun() ->
             AlgoList = [aes_ecb],
             PadTypeList = [zero, rfc5652],
             PadLen = 16,
             {ok, Key} = leo_ssec:gen_salt(32),
             Msg = <<"Test Binary Stream">>,
             MetaDataList = [#algo_metadata{algorithm = Algo,
                                            pad_type = PadType,
                                            pad_len = PadLen}
                             || Algo <- AlgoList, PadType <- PadTypeList],
             lists:foreach(fun(X) ->
                                   ?assertMatch({true, _},
                                                leo_ssec:verify_block_encryption(Key, Msg, X))
                           end, MetaDataList)
     end
    ].

%% Test 5
test_verify_ssec_algorithm() ->
    ?assertMatch({true,_},  leo_ssec:verify_ssec_algorithm("AES256")),
    ?assertMatch({false, _}, leo_ssec:verify_ssec_algorithm("AES")).

%% Test 6
test_verify_ssec_key() ->
    %% Key = "1234567890;1234567890;1234567890",
    %% Checksum = "B97C52E348AA77376E5472C96737671F",
    Base64Key1 = "MTIzNDU2Nzg5MDsxMjM0NTY3ODkwOzEyMzQ1Njc4OTA=",
    Base64Checksum1 = "uXxS40iqdzduVHLJZzdnHw==",
    ?assertMatch({true, _}, leo_ssec:verify_ssec_key(Base64Key1,
                                                     {md5, Base64Checksum1})),

    Base64Checksum2 = "5" ++ tl(Base64Checksum1),
    Base64Key2 = "5" ++ tl(Base64Key1),
    ?assertMatch({false, _}, leo_ssec:verify_ssec_key(Base64Key1,
                                                      {md5, Base64Checksum2})),
    ?assertMatch({false, _}, leo_ssec:verify_ssec_key(Base64Key2,
                                                      {md5, Base64Checksum1})),
    ?assertMatch({false, _}, leo_ssec:verify_ssec_key("ABCD" ++ Base64Key1,
                                                      {md5, Base64Checksum1})),
    %% safe to remove 2 ==
    ?assertMatch({false, _}, leo_ssec:verify_ssec_key(Base64Key1,
                                                      {md5, "ABCD" ++ Base64Checksum1})).

%% Test 7
-spec(verify_pad_unpad_test_() -> boolean()).
verify_pad_unpad_test_() ->
    [
     fun() ->
             %% No 0 at end (for zero padding)
             %% Test for empty also
             DataList = [<<1,2,3,4,5,6>>, <<1,2,3>>, <<123>>, <<>>],
             AlgoList = [zero, rfc5652],
             %% PadWidth can't be zero
             PadWidth = [2, 5, 8, 16],
             MetaList = [{X, Y, Z} || X <- DataList, Y <- AlgoList, Z <- PadWidth],
             lists:foreach(fun({Data, Algo, Z}) ->
                                   PadData = leo_ssec:pad(Algo, Z, Data),
                                   case Algo of
                                       rfc5652 ->
                                           ?assertNotEqual(Data, PadData),
                                           ?assertNotEqual(PadData, leo_ssec:pad(Algo, Z, PadData));
                                       zero ->
                                           ?assertEqual(PadData, leo_ssec:pad(Algo, Z, PadData))
                                   end,
                                   ?assertEqual(0, size(PadData) rem Z),
                                   ?assertEqual(Data, leo_ssec:unpad(Algo, PadData))
                           end, MetaList)
     end
    ].

%% Test 8
check_en_de(Len) ->
    RBin = crypto:strong_rand_bytes(Len),
    Key = crypto:strong_rand_bytes(32),
    {ok, Cipher, Hash, Salt} = leo_ssec:encrypt_object(RBin, Key),
    {ok, RBin} = leo_ssec:decrypt_object(Cipher, Key, Hash, Salt),
    ok.

-endif.
