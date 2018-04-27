%%======================================================================
%%
%% Leo Commons / LeoSSEC
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
-module(leo_ssec).
-author("kunal.tyagi").

-include("leo_commons.hrl").

-export([encrypt_object/2, decrypt_object/4]).
-export([gen_salt/1, gen_hash/3,
         verify_key/3,
         verify_block_encryption/3, verify_stream_encryption/3,
         block_encrypt_data/2, block_encrypt_data/3,
         block_encrypt_data/4,
         block_decrypt_data/2, block_decrypt_data/3,
         block_decrypt_data/4,
         stream_encrypt_data/3, stream_encrypt_data/4,
         stream_decrypt_data/3, stream_decrypt_data/4,
         verify_ssec_algorithm/1, verify_ssec_key/2
        ]).

-ifdef(TEST).
-export([pad/3, unpad/2]).
-endif.


%%-------------------------------------------------------------------------
%% API
%%-------------------------------------------------------------------------
%% @doc High Level Wrapper for SSE-C encryption
-spec(encrypt_object(Object, UserKey) ->
             {ok, Cipher, Hash, Salt} when Object::crypto:io_data(),
                                           UserKey::crypto:io_data(),
                                           Cipher::binary(),
                                           Hash::binary(),
                                           Salt::binary()).
encrypt_object(Object, UserKey) ->
    {ok, Salt} = gen_salt(32),
    <<IV:?IV_LEN/binary, _/binary>> = Salt,
    {sha256, Hash} = gen_hash(sha256, UserKey, Salt),
    Cipher = block_encrypt_data(UserKey, Object,
                                #algo_metadata{algorithm = aes_cbc256,
                                               pad_type = rfc5652,
                                               pad_len = ?IV_LEN}, IV),
    {ok, Cipher, Hash, Salt}.


%% @doc High Level Wrapper for SSE-C decryption
-spec(decrypt_object(Cipher, UserKey, Hash, Salt) ->
             {error, invalid}|{ok, Object}
                 when Object::crypto:io_data(),
                      UserKey::crypto:io_data(),
                      Cipher::binary(),
                      Hash::binary(),
                      Salt::binary()).
decrypt_object(Cipher, UserKey, Hash, Salt) ->
    case verify_key(UserKey, Salt, {sha256, Hash}) of
        false ->
            {error, invalid};
        true ->
            <<IV:?IV_LEN/binary, _/binary>> = Salt,
            Object = block_decrypt_data(UserKey, Cipher,
                                        #algo_metadata{algorithm = aes_cbc256,
                                                       pad_type = rfc5652,
                                                       pad_len = ?IV_LEN}, IV),
            {ok, Object}
    end.


%% @doc generate a random salt
-spec(gen_salt(Length) ->
             {ok, Salt} | {error, ErrorDescription} when Length::integer(),
                                                         Salt::binary(),
                                                         ErrorDescription::string()).
gen_salt(Length) when Length >= 32, Length =< 64 ->
    %% erlang:binary_to_list(crypto:strong_rand_bytes(Length)).
    {ok, crypto:strong_rand_bytes(Length)};
gen_salt(Length) ->
    {error, "Length must be between 32 and 64. Provided: " ++ erlang:integer_to_list(Length)}.


%% @doc generate hash of user key using the salt
-spec(gen_hash(Type, UserKey, Salt) ->
             {Type, Mac} when Type::crypto:hash_algorithms(),
                              %% except ripemd160,
                              UserKey::iodata(),
                              Salt::iodata(),
                              Mac::binary()).
gen_hash(Type, UserKey, Salt) ->
    %% save the algorithm used, eg md5, sha, sha256
    {Type, crypto:hmac(Type, Salt, UserKey)}.


%% @doc verify that the userkey and salt combination matches the provided hash
-spec(verify_key(UserKey, Salt, Hash) ->
             Status when UserKey::iodata(),
                         Salt::iodata(),
                         Hash::{Type, binary()},
                         %% except ripemd160
                         Type::crypto:hash_algorithms(),
                         Status::boolean()).
verify_key(UserKey, Salt, Hash) ->
    {HashType, _HashValue} = Hash,
    gen_hash(HashType, UserKey, Salt) =:= Hash.


%% @doc block encrypt data using the user key and default settings
-spec(block_encrypt_data(UserKey, Data) ->
             CipherPadData when UserKey::crypto:block_key(),
                                Data::crypto:io_data(),
                                CipherPadData::binary()).
block_encrypt_data(UserKey, Data) ->
    block_encrypt_data(UserKey, Data, #algo_metadata{}).

%% @doc block encrypt data using the user key and custom settings
-spec(block_encrypt_data(UserKey, Data, AlgoMetaData) ->
             CipherPadData when UserKey::crypto:block_key(),
                                Data::crypto:io_data(),
                                AlgoMetaData::#algo_metadata{},
                                CipherPadData::binary()).
block_encrypt_data(UserKey, Data, #algo_metadata{algorithm = Algo,
                                                 pad_type = PadType,
                                                 pad_len = PadLen}) ->
    PadData = pad(PadType, PadLen, Data),
    crypto:block_encrypt(Algo, UserKey, PadData).

%% @doc block encrypt data using key and custome settings with IV
-spec(block_encrypt_data(UserKey, Data, AlgoMetaData, IV) ->
             CipherPadData when UserKey::crypto:block_key(),
                                Data::crypto:io_data(),
                                AlgoMetaData::#algo_metadata{},
                                IV::crypto:io_data(),
                                CipherPadData::binary()).
block_encrypt_data(UserKey, Data, #algo_metadata{algorithm = Algo,
                                                 pad_type = PadType,
                                                 pad_len = PadLen}, IV) ->
    PadData = pad(PadType, PadLen, Data),
    crypto:block_encrypt(Algo, UserKey, IV, PadData).


%% @doc stream encrypt data using user key
-spec(stream_encrypt_data(State, Data) ->
             {State, CipherData} when State::crypto:opaque(),
                                      Data::crypto:io_data(),
                                      CipherData::binary()).
stream_encrypt_data(State, Data) ->
    crypto:stream_encrypt(State, Data).

%% @doc stream encrypt data using the user key and settings (no default)
-spec(stream_encrypt_data(UserKey, Data, AlgoMetaData) ->
             {State, CipherData} when UserKey::crypto:io_data(),
                                      Data::crypto:io_data(),
                                      AlgoMetaData::#algo_metadata{},
                                      State::{state, crypto:opaque()},
                                      CipherData::binary()).
stream_encrypt_data(UserKey, Data, #algo_metadata{algorithm = Algo}) ->
    stream_encrypt_data(crypto:stream_init(Algo, UserKey), Data).

%% @doc stream encrypt data using the user key and init vector
-spec(stream_encrypt_data(UserKey, Data, IVec, AlgoMetaData) ->
             {State, CipherData} when UserKey::crypto:io_data(),
                                      Data::crypto:io_data(),
                                      IVec::binary(),
                                      AlgoMetaData::#algo_metadata{},
                                      State::crypto:opaque(),
                                      CipherData::binary()).
stream_encrypt_data(UserKey, Data, IVec, #algo_metadata{algorithm = Algo}) ->
    stream_encrypt_data(crypto:stream_init(Algo, UserKey, IVec), Data).


%% @doc block decrypt data using the user key
-spec(block_decrypt_data(UserKey, Data) ->
             PlainData when UserKey::crypto:block_key(),
                            Data::binary(),
                            PlainData::iodata()).
block_decrypt_data(UserKey, Data) ->
    block_decrypt_data(UserKey, Data, #algo_metadata{}).

%% @doc block decrypt data using the user key
-spec(block_decrypt_data(UserKey, Data, AlgoMetaData) ->
             PlainData when UserKey::crypto:block_key(),
                            Data::binary(),
                            AlgoMetaData::#algo_metadata{},
                            PlainData::iodata()).
block_decrypt_data(UserKey, Data, #algo_metadata{algorithm = Algo,
                                                 pad_type = PadType}) ->
    PadData = crypto:block_decrypt(Algo, UserKey, Data),
    unpad(PadType, PadData).

%% @doc block decrypt data using key and custome settings with IV
-spec(block_decrypt_data(UserKey, Data, AlgoMetaData, IV) ->
             PlainData when UserKey::crypto:block_key(),
                            Data::binary(),
                            AlgoMetaData::#algo_metadata{},
                            IV::binary(),
                            PlainData::iodata()).
block_decrypt_data(UserKey, Data, #algo_metadata{algorithm = Algo,
                                                 pad_type = PadType}, IV) ->
    PadData = crypto:block_decrypt(Algo, UserKey, IV, Data),
    unpad(PadType, PadData).


%% @doc stream decrypt data using the user key
-spec(stream_decrypt_data(UserKey, Data, AlgoMetaData) ->
             {State, CipherData} when UserKey::{key, crypto:io_data()} | State,
                                      Data::crypto:io_data(),
                                      AlgoMetaData::#algo_metadata{},
                                      State::{state, crypto:opaque()},
                                      CipherData::binary()).
stream_decrypt_data({state, OldState}, Data) ->
    crypto:stream_decrypt(OldState, Data);
stream_decrypt_data({key, UserKey}, Data) ->
    stream_decrypt_data(UserKey, Data, #algo_metadata{}).
stream_decrypt_data({key, UserKey}, Data, #algo_metadata{algorithm = Algo}) ->
    stream_decrypt_data({state, crypto:stream_init(Algo, UserKey)}, Data).

%% @doc stream decrypt data using the user key and init vector
-spec(stream_decrypt_data(UserKey, Data, IVec, AlgoMetaData) ->
             {State, CipherData} when UserKey::{key, crypto:io_data()} | State,
                                      Data::crypto:io_data(),
                                      IVec::binary(),
                                      AlgoMetaData::#algo_metadata{},
                                      State::{state, crypto:opaque()},
                                      CipherData::binary()).
stream_decrypt_data({key, UserKey}, Data, IVec, #algo_metadata{algorithm = Algo}) ->
    stream_decrypt_data({state, crypto:stream_init(Algo, UserKey, IVec)}, Data).


%% @doc verify that block data is encrypted correctly
-spec(verify_block_encryption(Key, Msg, AlgoMetaData) ->
             {Status, EncryptedMsg} when Key::crypto:block_key(),
                                         Msg::crypto:io_data(),
                                         AlgoMetaData::#algo_metadata{},
                                         Status::boolean(),
                                         EncryptedMsg::binary()).
verify_block_encryption(Key, Msg, AlgoMetaData) ->
    EncryptedMsg = block_encrypt_data(Key, Msg, AlgoMetaData),
    {Msg =:= block_decrypt_data(
               Key, EncryptedMsg, AlgoMetaData), EncryptedMsg}.


%% @doc verify that stream data is encrypted correctly
%% @TODO implementation
-spec(verify_stream_encryption(_Key, _Msg, _AlgoMetaData) ->
             {Status, EncryptedMsg} when _Key::crypto:stream_key(),
                                         _Msg::crypto:io_data(),
                                         _AlgoMetaData::#algo_metadata{},
                                         Status::boolean(),
                                         EncryptedMsg::binary()).
verify_stream_encryption(_Key, _Msg, _AlgoMetaData) ->
    {false, "@TODO not implemented yet"}.


%% @doc verifies that the user demanded the correct algorithm only AES256 allowed
-spec(verify_ssec_algorithm(Algorithm) ->
             {Status, ValidAlgorithms} when Algorithm::string(),
                                            Status::boolean(),
                                            ValidAlgorithms::[string()]).
verify_ssec_algorithm(Algorithm) ->
    ValidAlgorithms = ["AES256"],
    {lists:member(Algorithm, ValidAlgorithms), ValidAlgorithms}.


%% @doc verifies that the user provided key matches the hash
%%      Key and hash are Base64 encoded
-spec(verify_ssec_key(ASCIIKey, Checksum) ->
             {Status, ErrorDescription} when ASCIIKey::string(),
                                             Checksum::{Type, ASCIIHash},
                                             Type::md5,
                                             ASCIIHash::string(),
                                             Status::boolean(),
                                             ErrorDescription::string()).
verify_ssec_key(ASCIIKey, Checksum) ->
    Key = base64:decode(ASCIIKey),
    {HashType, ASCIIHash} = Checksum,
    HashValue = base64:decode(ASCIIHash),
    verify_ssec_key_1(Key, HashType, HashValue).

verify_ssec_key_1(Key,_HashType,_HashValue) when size(Key) /= 256/8 ->
    {false, lists:append(["Key is not 256 bit long. Provided: ",
                          erlang:integer_to_list(size(Key))])};
verify_ssec_key_1(_Key, HashType,_HashValue) when HashType /= md5 ->
    {false, lists:append(["MD5 checksum required. Provided: ",
                          erlang:atom_to_list(HashType)])};
verify_ssec_key_1(Key, HashType,_HashValue) when size(HashType) /= 128/8 ->
    {false, lists:append(["MD5 checksum is not 128 bit long. Provided: ",
                          erlang:integer_to_list(size(Key))])};
verify_ssec_key_1(Key, HashType, HashValue) ->
    {crypto:hash(HashType, Key) =:= HashValue, "Verification status"}.


%%-------------------------------------------------------------------------
%% Private
%%-------------------------------------------------------------------------
%% @doc pad binary data
%% @private
-spec(pad(zero|rfc5652, Width, Binary) ->
             PaddedBinary when Width::integer(),
                               Binary::binary(),
                               PaddedBinary::binary()).
pad(_Type, 0, Binary) ->
    Binary;
pad(zero, Width, Binary) ->
    pad_zero(Width, Binary);
pad(rfc5652, Width, Binary) ->
    pad_rfc5652(Width, Binary).


%% @doc unpad binary data
%% @private
-spec(unpad(zero|rfc5652, Binary) ->
             UnpaddedBinary when Binary::binary(),
                                 UnpaddedBinary::binary()).
unpad(zero, Binary) ->
    unpad_zero(Binary);
unpad(rfc5652, Binary) ->
    unpad_rfc5652(Binary).


%% @doc pad data with zeroes at the end
%% @private
-spec(pad_zero(Width, Binary) ->
             PaddedBinary when Width::integer(),
                               Binary::binary(),
                               PaddedBinary::binary()).
pad_zero(Width, Binary) when Width /= 0 ->
    case (Width - (size(Binary) rem Width)) rem Width of
        0 ->
            Binary;
        N ->
            <<Binary/binary, 0:(N*8)>> % 8 bits in one byte
    end.


%% @doc unpad zeroes from end of data
%%      Handles empty binaries
%%      @see unpad_zero/2
%% @private
-spec(unpad_zero(Binary) ->
             UnpaddedBinary when Binary::binary(),
                                 UnpaddedBinary::binary()).
unpad_zero(Binary) ->
    unpad_zero(Binary, size(Binary) - 1).

%% @doc unpad zeroes by counting them
%% @private
-spec(unpad_zero(Binary, Idx) ->
             UnpaddedBinary when Binary::binary(),
                                 Idx::integer(),
                                 UnpaddedBinary::binary()).
unpad_zero(_Binary, -1) ->
    <<>>;
unpad_zero(Binary, Idx) when Idx > -1 ->
    case binary:at(Binary, Idx) of
        0 ->
            unpad_zero(Binary, Idx - 1);
        _ ->
            binary:part(Binary, 0, Idx + 1)
    end.

%% @doc pad data padded a per RFC5652
%%      @see pad_rfc5652/3
%% @private
-spec(pad_rfc5652(Width, Binary) ->
             PaddedBinary when Width::integer(),
                               Binary::binary(),
                               PaddedBinary::binary()).
pad_rfc5652(Width, Binary) when Width /= 0 ->
    case (Width - (size(Binary) rem Width)) rem Width of
        0 ->
            pad_rfc5652(Width, Width, Binary);
        N ->
            pad_rfc5652(N, N, Binary)
    end.

%% @doc pads by appending one byte at a time till Length bytes are added
%% @private
-spec(pad_rfc5652(OrigWidth, Length, Binary) ->
             PaddedBinary when OrigWidth::integer(),
                               Length::integer(),
                               Binary::binary(),
                               PaddedBinary::binary()).
pad_rfc5652(OrigWidth, Length, Binary) ->
    Suffix = binary:copy(<<OrigWidth:8>>, Length),
    <<Binary/binary, Suffix/binary>>.

%% @doc unpad data padded a per RFC5652
%%      Can't take empty bianries as input coz invalid input
%% @private
-spec(unpad_rfc5652(Binary) ->
             UnpaddedBinary when Binary::binary(),
                                 UnpaddedBinary::binary()).
unpad_rfc5652(Binary) ->
    Size = size(Binary),
    Last = binary:at(Binary, Size - 1),
    Suffix = binary:part(Binary, Size, -1 * Last),
    case lists:all(fun(X) ->
                           X =:= Last
                   end, binary:bin_to_list(Suffix)) of
        true ->
            binary:part(Binary, 0, Size - Last);
        false ->
            Binary
    end.
