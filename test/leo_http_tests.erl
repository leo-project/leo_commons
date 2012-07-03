%%====================================================================
%%
%% LeoFS Gateway
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
%% HTTP Util Test
%% @doc
%% @end
%%====================================================================
-module(leo_http_tests).
-author('Yoshiyuki Kanno').
-vsn('0.9.0').

-include("leo_commons.hrl").
-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%% TEST
%%--------------------------------------------------------------------
-ifdef(EUNIT).
api_test_() ->
    [
        fun key_inc_path_/0,
        fun key_eq_host1_/0,
        fun key_eq_host2_/0,
        fun key_inc_host1_/0
    ].
key_inc_path_() ->
    Host = ?S3_DEFAULT_ENDPOINT,
    Path = "/bucket/path_to_file.png",
    Ret = leo_http:key(Host, Path),
    "/" ++ Expected = Path,
    ?assertEqual(Expected, Ret).
key_eq_host1_() ->
    Host = "www.leofs.com",
    Path = "/images/path_to_file.png",
    Ret = leo_http:key(Host, Path),
    Expected = Host ++ Path,
    ?assertEqual(Expected, Ret).
key_eq_host2_() ->
    Host = "www.leofs.com",
    Path = "/www.leofs.com/path_to_file.png",
    Ret = leo_http:key(Host, Path),
    "/" ++ Expected = Path,
    ?assertEqual(Expected, Ret).
key_inc_host1_() ->
    Bucket = "bucket",
    Host = Bucket ++ "." ++ ?S3_DEFAULT_ENDPOINT,
    Path = "/path_to_file.png",
    Ret = leo_http:key(Host, Path),
    Expected = Bucket ++ Path,
    ?assertEqual(Expected, Ret).
-endif.
