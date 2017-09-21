-module(leo_ssec_base_tests).

-author("kunal.tyagi").

-include_lib("eunit/include/eunit.hrl").

-ifdef(TEST).
-spec test() -> term(). %% SRSLY can we do better?
-endif.

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
       {timeout, timer:seconds(1),
        fun() -> check_wrapper(fun check_salt/1, 1, 100) end}},
      {"test gen_hash/1 for salt size 32...64, key 64",
       {timeout, timer:seconds(1),
        fun() -> check_wrapper(fun check_hash/1, 32, 64) end}},
      {"test verify_key/1 for salt size 32...64, key 64",
       {timeout, timer:seconds(1),
        fun() -> check_wrapper(fun verify_key/1, 32, 64) end}},
      {"test verify_ssec_algorithm/1",
       {timeout, timer:seconds(1), fun test_verify_ssec_algorithm/0}},
      {"test verify_ssec_key/1",
       {timeout, timer:seconds(1), fun test_verify_ssec_key/0}}
     ]}.

%% Test 1
check_salt(Len) when Len >= 32, Len =< 64 ->
    ?assertMatch({ok, _}, leo_ssec_base:gen_salt(Len)),
    ?assertNotEqual(leo_ssec_base:gen_salt(Len), leo_ssec_base:gen_salt(Len));
check_salt(Len) ->
    ?assertMatch({error, _}, leo_ssec_base:gen_salt(Len)).

%% Test 2
check_hash(Len) ->
    AlgoList = [md5, sha, sha256],
    {ok, Key} = leo_ssec_base:gen_salt(64),
    {ok, Salt1} = leo_ssec_base:gen_salt(Len),
    {ok, Salt2} = leo_ssec_base:gen_salt(Len),
    % Check Hash algo actually generates hash
    lists:foreach(fun(Algo) -> ?assertEqual({Algo, crypto:hmac(Algo, Key, Salt1)}, leo_ssec_base:gen_hash(Algo, Salt1, Key)) end, AlgoList),
    % Check Hash is diff for diff salts
    HashList = lists:map(fun(Algo) -> {leo_ssec_base:gen_hash(Algo, Key, Salt1), leo_ssec_base:gen_hash(Algo, Key, Salt2)} end, AlgoList),
    lists:foreach(fun({Hash1, Hash2}) -> ?assertNotEqual(Hash1, Hash2) end,
                  HashList).

%% Test 3
verify_key(Len) ->
    AlgoList = [md5, sha, sha256],
    {ok, Key} = leo_ssec_base:gen_salt(64),
    {ok, Salt} = leo_ssec_base:gen_salt(Len),
    HashList = lists:map(fun(Algo) -> leo_ssec_base:gen_hash(Algo, Key, Salt) end, AlgoList),
    lists:foreach(fun(Hash) -> ?assert(leo_ssec_base:verify_key(Key, Salt, Hash)) end, HashList).


%% Test 4
-spec(verify_block_encryption_test_() -> boolean()).
verify_block_encryption_test_() ->
    [
     fun() ->
         AlgoList = [aes_ecb],
         PadType = [zero, rfc5652],
         PadLen = 16,
         {ok, Key} = leo_ssec_base:gen_salt(32),
         Msg = <<"Test Binary Stream">>,
         MetaDataList = [{Algo, Pad, PadLen} || Algo <- AlgoList, Pad <- PadType],
         lists:foreach(fun(X) ->
                           ?assertMatch({true, _},
                                        leo_ssec_base:verify_block_encryption(Key, Msg, X))
                       end, MetaDataList)
     end
    ].

%% Test 5
test_verify_ssec_algorithm() ->
    ?assertMatch({true,_},  leo_ssec_base:verify_ssec_algorithm("AES256")),
    ?assertMatch({false, _}, leo_ssec_base:verify_ssec_algorithm("AES")).

%% Test 6
test_verify_ssec_key() ->
    % Key = "1234567890;1234567890;1234567890",
    % Checksum = "B97C52E348AA77376E5472C96737671F",
    Base64Key1 = "MTIzNDU2Nzg5MDsxMjM0NTY3ODkwOzEyMzQ1Njc4OTA=",
    Base64Checksum1 = "uXxS40iqdzduVHLJZzdnHw==",
    ?assertMatch({true, _}, leo_ssec_base:verify_ssec_key(Base64Key1,
                                {md5, Base64Checksum1})),

    Base64Checksum2 = "5" ++ tl(Base64Checksum1),
    Base64Key2 = "5" ++ tl(Base64Key1),
    ?assertMatch({false, _}, leo_ssec_base:verify_ssec_key(Base64Key1,
                                {md5, Base64Checksum2})),
    ?assertMatch({false, _}, leo_ssec_base:verify_ssec_key(Base64Key2,
                                {md5, Base64Checksum1})),
    ?assertMatch({false, _}, leo_ssec_base:verify_ssec_key("ABCD" ++ Base64Key1,
                                {md5, Base64Checksum1})),
    % safe to remove 2 ==
    ?assertMatch({false, _}, leo_ssec_base:verify_ssec_key(Base64Key1,
                                {md5, "ABCD" ++ Base64Checksum1})).

%% Test 6
-spec(verify_pad_unpad_test_() -> boolean()).
verify_pad_unpad_test_() ->
    [
     fun() ->
         % No 0 at end (for zero padding)
         % Test for empty also
         DataList = [<<1,2,3>>, <<123>>, <<>>],
         AlgoList = [zero, rfc5652],
         % PadWidth can't be zero
         PadWidth = [2, 8, 16],
         MetaList = [{X, Y, Z} || X <- DataList, Y <- AlgoList, Z <- PadWidth],
         lists:foreach(fun({Data, Algo, Z}) ->
                           PadData = leo_ssec_base:pad(Algo, Z, Data),
                           ?assertNotEqual(Data, PadData),
                           ?assertEqual(Data, leo_ssec_base:unpad(Algo, PadData))
                       end, MetaList)
     end
    ].

-endif.
